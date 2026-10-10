import test from 'node:test';
import assert from 'node:assert/strict';
import { editorFixture } from './editor-fixture.mjs';

test('question attachments follow the reply, not the host queue notification', async t => {
  const { open } = await editorFixture(t);
  const { page, frame } = await open();
  await page.evaluate(() => {
    const apply = window.apply;
    window.apply = async args => {
      const result = await apply(args);
      if (args.action === 'ask') {
        window.questionQueued = true;
        await new Promise(resolve => { window.replyToQuestion = resolve; });
      }
      return result;
    };
  });
  await frame.locator('#ask-toggle').click();
  await frame.locator('#question').fill('Explain this board');
  await frame.locator('#ask').evaluate(form => {
    const transfer = new DataTransfer();
    transfer.items.add(new File(['notes'], 'notes.txt', { type: 'text/plain' }));
    form.dispatchEvent(new DragEvent('drop', { bubbles: true, cancelable: true, dataTransfer: transfer }));
  });
  await frame.locator('#question-attachments .attachment').waitFor();
  await frame.locator('#question-send').click();
  await page.waitForFunction(() => window.questionQueued);
  assert.equal(await frame.locator('#question-attachments .attachment').count(), 1);
  assert.equal(await frame.locator('#question-send').isDisabled(), true);
  await page.evaluate(() => window.replyToQuestion());
  await frame.locator('#question-attachments .attachment').waitFor({ state: 'hidden' });
  assert.equal(await frame.locator('#question').inputValue(), '');
});
