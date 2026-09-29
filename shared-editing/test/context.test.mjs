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

test('thread replies are attributed, retry-safe and included in explicit questions at the reviewed version', async () => {
  let {state} = await handle({action:'create',id:'doc',opId:'create',actor:'Alice',kind:'document',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'A data model.'}]}]}});
  const doc = load(state);
  try {
    const text = doc.getXmlFragment('document').get(0).get(0);
    const range = rangeFor(text,2,text,12), expected = captureContext(doc,{range}).snapshot;
    ({state} = await handle({action:'comment',state,opId:'thread',actor:'Guest: Alice',range,expected,text:'What does this mean?'}));
    const reply = {action:'reply-comment',opId:'reply',actor:'Guest: Bob',commentId:'thread',text:'Please include an example.'};
    ({state} = await handle({...reply,state}));
    assert.equal(state.comments[0].replies[0].actor,'Guest: Bob');
    assert.equal((await handle({...reply,state})).state.comments[0].replies.length,1);
    const ask = {action:'read',state,question:true,commentId:'thread',commentVersion:'reply',range,expected};
    const {result} = await handle(ask);
    assert.deepEqual(result.snapshot.discussion,[{actor:'Guest: Alice',text:'What does this mean?'},
      {actor:'Guest: Bob',text:'Please include an example.'}]);
    await assert.rejects(handle({...ask,commentVersion:'thread'}),/Discussion changed/);
    await assert.rejects(handle({...reply,state,opId:'empty',text:' '}),/reply is required/);
    await assert.rejects(handle({...reply,state,opId:'unknown',commentId:'missing'}),/no longer available/);
    ({state} = await handle({action:'resolve-comment',state,opId:'resolve',actor:'Bob',commentId:'thread',resolved:true}));
    await assert.rejects(handle({...reply,state,opId:'closed'}),/Reopen/);
    ({state} = await handle({action:'resolve-comment',state,opId:'reopen',actor:'Bob',commentId:'thread',resolved:false}));
    ({state} = await handle({...reply,state,opId:'reply-two',text:'And a diagram?'}));
    assert.equal(state.comments[0].replies.length,2);
    assert.deepEqual(result.snapshot.discussion.map(m=>m.text),['What does this mean?','Please include an example.']);
    const exported = await handle({action:'export',format:'native',state});
    assert.doesNotMatch(exported.result.text,/Please include|And a diagram/);
  } finally { doc.destroy(); }
});

test('board areas carry their region, nearby objects without image bytes and a cropped PNG', async () => {
  const image = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=';
  const {state} = await handle({action:'create',id:'board',opId:'create',actor:'Alice',kind:'whiteboard',content:[
    {id:'a',type:'rect',box:[0,0,100,100],text:'A'}, {id:'far',type:'ellipse',box:[2000,2000,100,100],text:'Far'},
    {id:'pic',type:'image',box:[150,0,100,100],src:image}]});
  const doc = load(state);
  const size = png => { const b = Buffer.from(png, 'base64'); return [b.readUInt32BE(16), b.readUInt32BE(20)]; };
  try {
    const region = [-10,-10,200,120];
    const captured = captureContext(doc, {selection:['a'], region});
    assert.deepEqual(captured.snapshot.region, region);
    assert.equal(captured.snapshot.scope, 'selection');
    assert.deepEqual(captured.snapshot.content.map(s => s.id), ['a']);
    assert.deepEqual(captured.snapshot.context.map(s => s.id), ['pic'], 'touched neighbours, not distant objects');
    assert.equal(captured.snapshot.context[0].src, undefined);
    assert.match(captured.snapshot.context[0].image, /PNG/);
    assert.match(captured.quote, /^Area 200 × 120 at -10, -10\n1 object\nrect: A$/);
    const empty = captureContext(doc, {region:[500,500,50,40]});
    assert.deepEqual([empty.snapshot.content, empty.snapshot.context], [[], []]);
    assert.match(empty.quote, /^Area 50 × 40 at 500, 500\n0 objects$/);
    for (const bad of [[0,0,0,10], [0.5,0,10,10], [0,0,10], 'area'])
      assert.throws(() => captureContext(doc, {region:bad}), /Invalid board area/);
    const {result} = await handle({action:'read',state,question:true,selection:['a'],region,
      expected:captured.snapshot,image:true,imageMax:1024});
    assert.deepEqual(result.snapshot.region, region);
    assert.deepEqual(size(result.png), [992, 672], 'area plus margin, upscaled to stay legible');
    const whole = await handle({action:'read',state,question:true,selection:['a'],
      expected:captureContext(doc, {selection:['a']}).snapshot,image:true});
    assert.deepEqual(size(whole.result.png), [160, 160], 'object questions keep their own framing');
    await assert.rejects(handle({action:'read',state,question:true,selection:['a'],region:[0,0,10,10],
      expected:captured.snapshot}), /Content changed/);
  } finally { doc.destroy(); }
});

test('board comments anchor to objects and areas, track changes and send surviving objects', async () => {
  let {state} = await handle({action:'create',id:'board',opId:'create',actor:'Alice',kind:'whiteboard',content:[
    {id:'a',type:'rect',box:[0,0,100,100],text:'A'}, {id:'b',type:'rect',box:[300,0,100,100],text:'B'}]});
  const commit = async (edit) => {
    const doc = load(state);
    try {
      edit(doc.getMap('shapes'));
      ({state} = await handle({action:'update',opId:crypto.randomUUID(),actor:'Guest: Bob',state,
        update:Buffer.from(Y.encodeStateAsUpdate(doc)).toString('base64')}));
    } finally { doc.destroy(); }
  };
  const statuses = () => { const doc = load(state); try { return readComments(doc, state.comments); } finally { doc.destroy(); } };
  let doc = load(state);
  const objects = captureContext(doc, {selection:['a','b']}).snapshot;
  const area = captureContext(doc, {region:[500,0,80,60]}).snapshot;
  doc.destroy();
  ({state} = await handle({action:'comment',state,opId:'objects',actor:'Guest: Alice',text:'Align these',
    selection:['a','b'],expected:objects}));
  ({state} = await handle({action:'comment',state,opId:'area',actor:'Guest: Alice',text:'Legend here',
    selection:[],region:[500,0,80,60],expected:area}));
  await assert.rejects(handle({action:'comment',state,opId:'none',actor:'Guest: Alice',text:'?',selection:[],
    expected:objects}), /Select objects or an area/);
  assert.deepEqual(state.comments[0].selection, ['a','b']);
  assert.match(state.comments[0].signature, /^[0-9a-f]{16}$/);
  assert.equal(state.comments[0].quote, '2 objects\nrect: A\nrect: B');
  assert.deepEqual(state.comments[1].region, [500,0,80,60]);
  assert.deepEqual(statuses().map(c => c.anchorStatus), ['current','current']);
  await commit(shapes => shapes.get('b').set('text', 'B2'));
  assert.deepEqual(statuses().map(c => c.anchorStatus), ['changed','current'], 'an edited object changes its thread');
  await commit(shapes => shapes.delete('b'));
  const [thread] = statuses();
  assert.equal(thread.anchorStatus, 'changed');
  assert.deepEqual(thread.liveSelection, ['a']);
  assert.equal(thread.liveQuote, '1 object\nrect: A');
  doc = load(state);
  const surviving = captureContext(doc, {selection:thread.liveSelection}).snapshot;
  doc.destroy();
  const ask = {action:'read',state,question:true,commentId:'objects',commentVersion:'objects',
    selection:['a'],expected:surviving};
  const {result} = await handle(ask);
  assert.deepEqual(result.snapshot.discussion, [{actor:'Guest: Alice',text:'Align these'}]);
  await assert.rejects(handle({...ask,selection:['a','intruder']}), /no longer available/);
  await assert.rejects(handle({...ask,commentId:'area',commentVersion:'area',region:[0,0,10,10]}), /no longer available/);
  await commit(shapes => shapes.delete('a'));
  assert.deepEqual(statuses().map(c => c.anchorStatus), ['deleted','current'], 'an area outlives its objects');
});

test('room questions about a whole item capture current content without a reviewed snapshot', async () => {
  let {state} = await handle({action:'create',id:'board',opId:'create',actor:'Guest: Alice',kind:'whiteboard',
    content:[{id:'a',type:'rect',box:[0,0,100,100],text:'A'},{id:'b',type:'ellipse',box:[200,0,100,100],text:'B'}]});
  const asked = await handle({action:'read',question:true,whole:true,state});
  assert.equal(asked.result.snapshot.scope, 'whole');
  assert.deepEqual(asked.result.snapshot.content.map(shape => shape.id), ['a','b']);
  assert.match(asked.result.quote, /^2 objects/);
  // Without the whole-item flag a question still needs the snapshot its
  // sender reviewed, and the flag never combines with a selection.
  await assert.rejects(handle({action:'read',question:true,state}), /Content changed/);
  await assert.rejects(handle({action:'read',question:true,whole:true,selection:['a'],state}),
                       /whole-item question takes no selection/);
});
