import test from 'node:test';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {once} from 'node:events';

test('helper frames fragmented Unicode requests and multiple lines', async t => {
  const child=spawn(process.execPath,['--no-experimental-webstorage',new URL('../host-entry.mjs',import.meta.url).pathname]);
  t.after(()=>child.kill());
  let output=''; child.stdout.setEncoding('utf8'); child.stdout.on('data',chunk=>output+=chunk);
  const requests=[
    {requestId:1,action:'create',id:'one',kind:'whiteboard',title:'λ 🌱',actor:'Alice',opId:'one'},
    {requestId:2,action:'read'},
  ];
  const bytes=Buffer.from(requests.map(x=>JSON.stringify(x)).join('\n')+'\n');
  // Single-byte writes include splits inside UTF-8 code points.
  for(const byte of bytes.subarray(0,95)) child.stdin.write(Buffer.from([byte]));
  child.stdin.end(bytes.subarray(95));
  const [code]=await once(child,'exit');
  assert.equal(code,0);
  const replies=output.trim().split('\n').map(JSON.parse);
  assert.deepEqual(replies.map(x=>x.requestId),[1,2]);
  assert.equal(replies[0].result.title,'λ 🌱');
  assert.match(replies[1].error,/Invalid persisted editor/);
});

test('helper bounds unfinished input before its newline arrives', async t => {
  const child=spawn(process.execPath,['--no-experimental-webstorage',new URL('../host-entry.mjs',import.meta.url).pathname]);
  t.after(()=>child.kill());
  child.stdin.on('error',()=>{});
  const exited=once(child,'exit');
  child.stdin.end('x'.repeat(64*1024*1024+1));
  assert.equal((await exited)[0],2);
});
