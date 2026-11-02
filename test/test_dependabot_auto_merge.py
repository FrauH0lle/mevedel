"""Exercise the privileged workflow against metadata-only GitHub responses."""

import copy
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import textwrap
import unittest


ROOT = Path(__file__).resolve().parent.parent
HEAD = 'a' * 40
MERGE = 'b' * 40
REPO = 'FrauH0lle/mevedel'

FAKE_GH = r'''
import json, os, pathlib, subprocess, sys
args = sys.argv[1:]
fixture = pathlib.Path(os.environ['GH_FIXTURE'])
data = json.loads(fixture.read_text())
with open(os.environ['GH_CALLS'], 'a') as stream:
    stream.write(json.dumps(args) + '\n')
if args[0] == 'api':
    if '--method' in args:
        assert args[args.index('--method') + 1] == 'GET'
    endpoint = next(arg for arg in args if arg.startswith('repos/'))
    if endpoint.endswith('/pulls'):
        value = [] if 'state=open' in args and data['view']['state'] != 'OPEN' else data['pulls']
    elif '/git/ref/heads/master' in endpoint:
        value = {'object': {'sha': data['master']}}
    elif '/actions/workflows/' in endpoint:
        workflow = endpoint.split('/workflows/')[1].split('/')[0]
        assert 'head_sha=' + data['master'] in args
        value = {'total_count': data['existing'].get(workflow, 0)}
    else:
        raise SystemExit('Unexpected endpoint: ' + endpoint)
elif args[:2] == ['pr', 'view']:
    if data.get('view_error'):
        raise SystemExit(1)
    value = data['view']
elif args[:2] == ['pr', 'merge']:
    if data['merge_now']:
        data['view'].update(state='MERGED', mergeCommit={'oid': data['merge']})
        fixture.write_text(json.dumps(data))
    raise SystemExit(1 if data.get('merge_error') else 0)
elif args[:2] == ['workflow', 'run']:
    assert args[3:] == ['--ref', 'master']
    raise SystemExit(0)
else:
    raise SystemExit('Unexpected command: ' + repr(args))
encoded = json.dumps(value)
if '--jq' in args:
    result = subprocess.run(['jq', '-r', args[args.index('--jq') + 1]],
                            input=encoded, text=True, capture_output=True, check=True)
    sys.stdout.write(result.stdout)
else:
    print(encoded)
'''


class AutoMergeWorkflowTests(unittest.TestCase):
    def fixture(self):
        return dict(
            pulls=[dict(user=dict(login='dependabot[bot]'),
                        head=dict(sha=HEAD, repo=dict(full_name=REPO)),
                        base=dict(ref='master', repo=dict(full_name=REPO)),
                        html_url='https://github.com/' + REPO + '/pull/19')],
            view=dict(state='OPEN', mergeCommit=None,
                      files=[dict(path='relay/viewer/shared-editor.js')]),
            master=MERGE, merge=MERGE, merge_now=True, existing={})

    def run_workflow(self, data, expected_code=0):
        workflow = (ROOT / '.github/workflows/dependabot-auto-merge.yml').read_text()
        script = textwrap.dedent(workflow.split('        run: |\n', 1)[1])
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory)
            (path / 'fixture.json').write_text(json.dumps(data))
            gh = path / 'gh'
            gh.write_text('#!' + sys.executable + '\n' + FAKE_GH)
            gh.chmod(0o755)
            env = dict(os.environ, PATH=str(path) + os.pathsep + os.environ['PATH'],
                       GH_FIXTURE=str(path / 'fixture.json'), GH_CALLS=str(path / 'calls'),
                       GH_REPO=REPO, REPO_OWNER='FrauH0lle', HEAD_BRANCH='dependabot/test',
                       HEAD_SHA=HEAD, GH_TOKEN='test-only')
            result = subprocess.run(['bash', '-euo', 'pipefail', '-c', script],
                                    cwd=path, env=env, capture_output=True, text=True, timeout=10)
            self.assertEqual(result.returncode, expected_code, result.stderr)
            calls = [json.loads(line) for line in (path / 'calls').read_text().splitlines()]
        merges = [call for call in calls if call[:2] == ['pr', 'merge']]
        dispatches = [call[2] for call in calls if call[:2] == ['workflow', 'run']]
        return merges, dispatches

    def test_queues_without_dispatching_before_merge(self):
        data = self.fixture()
        data['merge_now'] = False
        merges, dispatches = self.run_workflow(data)
        self.assertEqual(len(merges), 1)
        self.assertIn('--auto', merges[0])
        self.assertEqual(dispatches, [])

    def test_dispatches_checks_and_changed_relay_after_merge(self):
        merges, dispatches = self.run_workflow(self.fixture())
        self.assertEqual(len(merges), 1)
        self.assertEqual(dispatches, ['test.yml', 'dependencies.yml', 'relay.yml'])

    def test_handles_already_merged_events_and_preserves_existing_runs(self):
        data = self.fixture()
        data['view'].update(state='MERGED', mergeCommit=dict(oid=MERGE))
        data['existing'] = {'test.yml': 1, 'relay.yml': 1}
        merges, dispatches = self.run_workflow(data)
        self.assertEqual(merges, [])
        self.assertEqual(dispatches, ['dependencies.yml'])

    def test_does_not_release_an_unchanged_relay(self):
        data = self.fixture()
        data['view']['files'] = [dict(path='shared-editing/package-lock.json')]
        self.assertEqual(self.run_workflow(data)[1], ['test.yml', 'dependencies.yml'])

    def test_release_workflow_changes_also_request_a_release(self):
        data = self.fixture()
        data['view']['files'] = [dict(path='.github/workflows/relay.yml')]
        self.assertEqual(self.run_workflow(data)[1], ['test.yml', 'dependencies.yml', 'relay.yml'])

    def test_checks_latest_master_if_it_advanced_after_merge(self):
        data = self.fixture()
        data['master'] = 'c' * 40
        self.assertEqual(self.run_workflow(data)[1], ['test.yml', 'dependencies.yml', 'relay.yml'])

    def test_ignores_stale_human_fork_and_other_base_metadata(self):
        original = self.fixture()
        for change in ('stale', 'human', 'fork', 'base'):
            with self.subTest(change=change):
                data = copy.deepcopy(original)
                pr = data['pulls'][0]
                if change == 'stale':
                    pr['head']['sha'] = 'd' * 40
                elif change == 'human':
                    pr['user']['login'] = 'someone'
                elif change == 'fork':
                    pr['head']['repo']['full_name'] = 'someone/mevedel'
                else:
                    pr['base']['ref'] = 'another-branch'
                self.assertEqual(self.run_workflow(data), ([], []))

    def test_ignores_closed_unmerged_prs(self):
        data = self.fixture()
        data['view']['state'] = 'CLOSED'
        self.assertEqual(self.run_workflow(data), ([], []))

    def test_merge_failure_remains_visible(self):
        data = self.fixture()
        data.update(merge_now=False, merge_error=True)
        self.assertEqual(self.run_workflow(data, expected_code=1)[1], [])

    def test_metadata_read_failure_remains_visible(self):
        data = self.fixture()
        data['view_error'] = True
        self.assertEqual(self.run_workflow(data, expected_code=1), ([], []))

    def test_native_merge_racing_the_cli_still_dispatches(self):
        data = self.fixture()
        data['merge_error'] = True
        self.assertEqual(self.run_workflow(data)[1], ['test.yml', 'dependencies.yml', 'relay.yml'])


if __name__ == '__main__':
    unittest.main()
