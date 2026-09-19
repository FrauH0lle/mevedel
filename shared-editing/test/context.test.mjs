import test from 'node:test';
import assert from 'node:assert/strict';
import * as Y from 'yjs';
import { handle } from '../host.mjs';
import { restore } from '../model.mjs';
import { captureContext, checkContext, readComments } from '../context.mjs';
import { selectionPositions } from '../document.mjs';
const load = state => restore(Buffer.from(state.crdt, 'base64'));
const rangeFor = (start, from, end, to) => ({
  anchor:Y.relativePositionToJSON(Y.createRelativePositionFromTypeIndex(start, from, -1)),
  head:Y.relativePositionToJSON(Y.createRelativePositionFromTypeIndex(end, to, -1)),
});

test('comments retain original quotes while live anchors move, change and disappear', async () => {
  let {state} = await handle({action:'create',id:'doc',opId:'create',actor:'Guest: Alice',kind:'document',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'A useful passage.'}]}]}});
  const doc = load(state), text = doc.getXmlFragment('document').get(0).get(0);
  try {
    const range = rangeFor(text, 2, text, 8);
    assert.deepEqual(selectionPositions(doc, range), [3,9]);
    const captured = captureContext(doc, {range});
    assert.equal(captured.quote, 'useful');
    const request = {action:'comment',opId:'comment-a',actor:'Guest: Alice',text:'Explain this',range,expected:captured.snapshot};
    ({state} = await handle({...request, state}));
    assert.equal(state.comments[0].quote, 'useful');
    assert.equal(state.comments[0].actor, 'Guest: Alice');
    const duplicate = await handle({...request, state});
    assert.equal(duplicate.state.comments.length, 1);
    assert.equal(duplicate.state.revision, state.revision);
    text.insert(0, 'Prefix ');
    assert.equal(readComments(doc, state.comments)[0].anchorStatus, 'current');
    text.insert(11, 'very ');
    assert.equal(readComments(doc, state.comments)[0].anchorStatus, 'changed');
    assert.throws(() => checkContext(doc, {range,expected:captured.snapshot}), /Content changed/);
    ({state} = await handle({action:'update',opId:'edit',actor:'Guest: Bob',state,update:Buffer.from(Y.encodeStateAsUpdate(doc)).toString('base64')}));
    const read = await handle({action:'read',state});
    assert.equal(read.result.comments[0].quote, 'useful');
    assert.match(read.result.comments[0].liveQuote, /very/);
    ({state} = await handle({action:'resolve-comment',state,opId:'resolve',actor:'Guest: Bob',commentId:'comment-a',resolved:true}));
    assert.equal(state.comments[0].resolved, true);
    text.delete(0, text.length);
    assert.equal(readComments(doc, state.comments)[0].anchorStatus, 'deleted');
    assert.throws(() => captureContext(doc, {range}), /no longer available/);
    const exported = await handle({action:'export',format:'native',state});
    assert.doesNotMatch(exported.result.text, /Explain this|comment-a|Guest: Alice/);
  } finally { doc.destroy(); }
});

test('cross-block and reversed selections capture exact text and committed item identity', async () => {
  const {state} = await handle({action:'create',id:'doc',opId:'create',actor:'Alice',kind:'document',title:'Design notes',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'First passage'}]},
      {type:'paragraph',attrs:{id:'b'},content:[{type:'text',text:'Second passage'}]}]}});
  const doc = load(state);
  try {
    const root = doc.getXmlFragment('document');
    const range = rangeFor(root.get(1).get(0), 6, root.get(0).get(0), 6);
    const captured = captureContext(doc, {range});
    assert.equal(captured.quote, 'passage\nSecond');
    const {result} = await handle({action:'read',state,range,question:true,expected:captured.snapshot});
    assert.equal(result.snapshot.id, 'doc');
    assert.equal(result.snapshot.title, 'Design notes');
    assert.equal(result.snapshot.revision, state.revision);
    assert.equal(result.snapshot.content.text, 'passage\nSecond');
    assert.equal(result.snapshot.scope, 'selection');
    assert.equal(result.snapshot.content.anchors, undefined);
    await assert.rejects(handle({action:'read',state,question:true,expected:captured.snapshot}), /Content changed/);
    await assert.rejects(handle({action:'comment',state,actor:'Alice',opId:'bad',range,text:'',expected:captured.snapshot}), /comment is required/);
  } finally { doc.destroy(); }
});

test('board context keeps selection identities, rejects deleted targets and bounds large context', async () => {
  const {state} = await handle({action:'create',id:'board',opId:'create',actor:'Alice',kind:'whiteboard',content:[
    {id:'a',type:'rect',box:[0,0,100,100],text:'A'}, {id:'b',type:'ellipse',box:[200,0,100,100],text:'B'}]});
  const doc = load(state);
  try {
    assert.match(captureContext(doc, {selection:['a']}).quote, /^1 object\n/);
    const captured = captureContext(doc, {selection:['a','b']});
    assert.equal(captured.snapshot.content.length, 2);
    assert.match(captured.quote, /rect: A\nellipse: B/);
    const {result} = await handle({action:'read',state,question:true,selection:['a','b'],expected:captured.snapshot,image:true});
    assert.ok(result.png.length > 100);
    assert.throws(() => captureContext(doc, {selection:['missing']}), /no longer available/);
  } finally { doc.destroy(); }
  const large = await handle({action:'create',id:'large',opId:'create',actor:'Alice',kind:'document',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'x'.repeat(140000)}]}]}});
  const largeDoc = load(large.state);
  try {
    assert.throws(() => captureContext(largeDoc), /too large/);
    const text = largeDoc.getXmlFragment('document').get(0).get(0);
    assert.equal(captureContext(largeDoc, {range:rangeFor(text,0,text,3)}).quote, 'xxx');
  } finally { largeDoc.destroy(); }
});
