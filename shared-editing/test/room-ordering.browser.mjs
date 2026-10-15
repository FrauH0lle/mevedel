import test from 'node:test';
import assert from 'node:assert/strict';
import { editorFixture } from './editor-fixture.mjs';
import { shown, box, openWhole } from './sidebar.mjs';

test('message attachments follow the reply, not the host queue notification', async t => {
  const { open } = await editorFixture(t);
  const { page, frame } = await open();
  await page.evaluate(() => {
    const apply = window.apply;
    window.apply = async args => {
      const result = await apply(args);
      if (args.action === 'discussion' && args.toAssistant) {
        window.questionQueued = true;
        await new Promise(resolve => { window.replyToQuestion = resolve; });
      }
      return result;
    };
  });
  await openWhole(frame);
  await box(frame).fill('Explain this board');
  await shown(frame).locator('.ds-compose').evaluate(form => {
    const transfer = new DataTransfer();
    transfer.items.add(new File(['notes'], 'notes.txt', { type: 'text/plain' }));
    form.dispatchEvent(new DragEvent('drop', { bubbles: true, cancelable: true, dataTransfer: transfer }));
  });
  const files = shown(frame).locator('.ds-files .attachment');
  await files.waitFor();
  await shown(frame).locator('.ds-send').click();
  await page.waitForFunction(() => window.questionQueued);
  assert.equal(await files.count(), 1);
  assert.equal(await shown(frame).locator('.ds-send').isDisabled(), true);
  await page.evaluate(() => window.replyToQuestion());
  await files.waitFor({ state: 'hidden' });
  assert.equal(await box(frame).inputValue(), '');
});
