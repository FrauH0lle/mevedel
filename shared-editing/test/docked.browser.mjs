import test from 'node:test';
import assert from 'node:assert/strict';
import {editorFixture} from './editor-fixture.mjs';
import {askBox, box, send, shown} from './sidebar.mjs';

test('docked editors share the whole-item discussion with chat without losing sidebar drafts', async t => {
  const {open} = await editorFixture(t);
  for (const kind of ['whiteboard', 'document']) await t.test(kind, async () => {
    const {page, frame} = await open({kind, docked:true});
    const general = frame.locator('.ds-item.general');
    await general.waitFor();
    assert.equal(await frame.locator('#assistant').isVisible(), true, 'docked sidebar starts open');
    assert.match(await general.innerText(), /in chat ←/);
    await general.click();
    await page.waitForFunction(() => window.messages.some(m => m.type === 'focus-chat'));
    assert.equal(await shown(frame).isVisible(), false, 'general row does not open a second composer');
    await page.evaluate(async () => {
      const result = await window.apply({action:'discussion-post', opId:'chat-message',
        messageId:'chat-message', target:{kind:'whole'}, text:'Activity in room chat'});
      window.port.postMessage({type:'changed', ...result});
    });
    await general.locator('.ds-snip').getByText('Alice: Activity in room chat', {exact:true}).waitFor();
    assert.equal(await general.evaluate(node => node.classList.contains('unread')), false,
      'the adjacent chat does not also signal unread general-discussion news in the sidebar');

    // Shrinking the room makes this same iframe full screen; its whole-item
    // discussion must then be reachable without a hidden chat composer.
    await page.evaluate(() => window.port.postMessage({type:'docked', docked:false}));
    await general.locator('.ds-meta', {hasText:'in chat ←'}).waitFor({state:'hidden'});
    await general.click();
    await box(frame).fill('A whole-item draft');
    assert.equal(await box(frame).inputValue(), 'A whole-item draft');
    await page.evaluate(() => window.port.postMessage({type:'docked', docked:true}));
    await general.waitFor();
    await general.getByText('in chat ←', {exact:true}).waitFor();
    assert.equal(await shown(frame).isVisible(), false, 'redocking returns the whole-item discussion to chat');
    await page.evaluate(() => window.port.postMessage({type:'docked', docked:false}));
    await general.locator('.ds-meta', {hasText:'in chat ←'}).waitFor({state:'hidden'});
    await general.click();
    assert.equal(await box(frame).inputValue(), 'A whole-item draft', 'full-screen draft survives dock transitions');

    // A discussion about a part stays in the sidebar, with its active draft,
    // when only the destination of the whole-item row changes.
    if (kind === 'whiteboard') await frame.locator('[data-shape="ellipse"]').click();
    else {
      await frame.locator('.tiptap').fill('Some document content');
      await frame.locator('html[data-save-state=saved]').waitFor({state:'attached'});
      await frame.locator('.tiptap').evaluate(node => node.editor.commands.setTextSelection({from:1, to:5}));
    }
    await frame.locator('#discuss-selection').click();
    await box(frame).fill('A specific part');
    await askBox(frame).uncheck();
    await send(frame);
    await shown(frame).locator('.ds-message').getByText('A specific part', {exact:true}).waitFor();
    await box(frame).fill('A part-specific draft');
    for (const docked of [true, false]) {
      await page.evaluate(docked => window.port.postMessage({type:'docked', docked}), docked);
      assert.equal(await box(frame).inputValue(), 'A part-specific draft');
      assert.match(await shown(frame).innerText(), /A specific part/);
    }
    await page.close();
  });
});
