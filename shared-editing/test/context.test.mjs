import test from 'node:test';
import assert from 'node:assert/strict';
import * as Y from 'yjs';
import { handle } from '../host.mjs';
import { restore } from '../model.mjs';
import { captureContext, checkContext, readDiscussions } from '../context.mjs';
import { selectionPositions } from '../document.mjs';
import { contentHash } from '../view.mjs';
const load = state => restore(Buffer.from(state.crdt, 'base64'));
/* A shape and, with TEXT, its bound label. */
const shape = (id, x, text, type = 'rectangle', y = 0) => [{id, type, x, y, width:100, height:100},
  ...(text ? [{id:`label-${id}`, type:'text', x, y, width:0, height:0, text, containerId:id}] : [])];
const rangeFor = (start, from, end, to) => ({
  anchor:Y.relativePositionToJSON(Y.createRelativePositionFromTypeIndex(start, from, -1)),
  head:Y.relativePositionToJSON(Y.createRelativePositionFromTypeIndex(end, to, -1)),
});

test('discussions retain original quotes while live anchors move, change and disappear', async () => {
  let {state} = await handle({action:'create',id:'doc',opId:'create',actor:'Guest: Alice',kind:'document',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'A useful passage.'}]}]}});
  const doc = load(state), text = doc.getXmlFragment('document').get(0).get(0);
  try {
    const range = rangeFor(text, 2, text, 8);
    assert.deepEqual(selectionPositions(doc, range), [3,9]);
    const captured = captureContext(doc, {range});
    assert.equal(captured.quote, 'useful');
    const request = {action:'discussion-post',opId:'message-a',messageId:'message-a',discussionId:'discussion-a',
      actor:'Alice',text:'Explain this',target:{kind:'passage',range},expected:contentHash(captured.snapshot)};
    ({state} = await handle({...request, state}));
    assert.equal(state.discussions[0].quote, 'useful');
    assert.deepEqual(state.discussions[0].target, {kind:'passage', range});
    assert.equal(state.discussions[0].messages[0].actor, 'Alice');
    assert.equal(state.discussions[0].messages[0].id, 'message-a');
    const duplicate = await handle({...request, state});
    assert.equal(duplicate.state.discussions.length, 1);
    assert.equal(duplicate.state.revision, state.revision);
    text.insert(0, 'Prefix ');
    assert.equal(readDiscussions(doc, state.discussions)[0].anchorStatus, 'current');
    text.insert(11, 'very ');
    assert.equal(readDiscussions(doc, state.discussions)[0].anchorStatus, 'changed');
    assert.throws(() => checkContext(doc, {range,expected:contentHash(captured.snapshot)}), /Content changed/);
    ({state} = await handle({action:'update',opId:'edit',actor:'Guest: Bob',state,update:Buffer.from(Y.encodeStateAsUpdate(doc)).toString('base64')}));
    const read = await handle({action:'read',state});
    assert.equal(read.result.discussions[0].quote, 'useful');
    assert.match(read.result.discussions[0].liveQuote, /very/);
    ({state} = await handle({action:'discussion-resolve',state,opId:'resolve',actor:'Bob',discussionId:'discussion-a',resolved:true}));
    assert.equal(state.discussions[0].resolved, true);
    assert.equal(state.discussions[0].resolvedBy, 'Bob');
    ({state} = await handle({action:'discussion-resolve',state,opId:'reopen',actor:'Bob',discussionId:'discussion-a',resolved:false}));
    assert.equal(state.discussions[0].resolved, false);
    assert.equal(state.discussions[0].resolvedBy, undefined);
    text.delete(0, text.length);
    assert.equal(readDiscussions(doc, state.discussions)[0].anchorStatus, 'deleted');
    assert.throws(() => captureContext(doc, {range}), /no longer available/);
    const exported = await handle({action:'export',format:'native',state});
    assert.doesNotMatch(exported.result.text, /Explain this|discussion-a|Alice/);
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
    const {result} = await handle({action:'read',state,range,question:true,expected:contentHash(captured.snapshot)});
    assert.equal(result.snapshot.id, 'doc');
    assert.equal(result.snapshot.title, 'Design notes');
    assert.equal(result.snapshot.revision, state.revision);
    assert.equal(result.snapshot.content.text, 'passage\nSecond');
    assert.equal(result.snapshot.scope, 'selection');
    assert.equal(result.snapshot.content.anchors, undefined);
    // The model reads the selection and the blocks it spans as editable lines.
    assert.deepEqual(result.prompt.split('\n').slice(0, 4), [
      'document "Design notes" at shared://doc, revision 1; the question is about a selection.',
      'Selected text:', '"passage\\nSecond"', 'Blocks containing it (HASH BLOCK-JSON):']);
    assert.match(result.prompt, /\n[0-9a-f]{12} \{"type":"paragraph","attrs":\{"id":"a"\}.*\n[0-9a-f]{12} \{"type":"paragraph","attrs":\{"id":"b"\}/);
    await assert.rejects(handle({action:'read',state,question:true,expected:contentHash(captured.snapshot)}), /Content changed/);
    await assert.rejects(handle({action:'discussion-post',state,actor:'Alice',opId:'bad',messageId:'bad',discussionId:'d',
      target:{kind:'passage',range},text:'',expected:contentHash(captured.snapshot)}), /message is required/);
  } finally { doc.destroy(); }
});

test('board context keeps selection identities, rejects deleted targets and bounds large context', async () => {
  const {state} = await handle({action:'create',id:'board',opId:'create',actor:'Alice',kind:'whiteboard',content:[
    ...shape('a', 0, 'A'), ...shape('b', 200, 'B', 'ellipse')]});
  const doc = load(state);
  try {
    assert.match(captureContext(doc, {selection:['a']}).quote, /^1 object\n/);
    const captured = captureContext(doc, {selection:['a','b']});
    assert.equal(captured.snapshot.content.length, 2);
    assert.deepEqual(captured.snapshot.context.map(e => e.id), ['label-a','label-b'], 'labels give the selection its meaning');
    assert.match(captured.quote, /rectangle: A\nellipse: B/);
    const {result} = await handle({action:'read',state,question:true,selection:['a','b'],expected:contentHash(captured.snapshot),image:true});
    assert.ok(result.png.length > 100);
    const prompt = result.prompt.split('\n');
    assert.equal(prompt[0], 'whiteboard "Whiteboard" at shared://board, revision 1; the question is about a selection.');
    assert.equal(prompt[1], 'Selected (HASH ELEMENT-JSON):');
    assert.match(prompt[2], /^[0-9a-f]{12} \{"id":"a","type":"rectangle"/);
    assert.equal(prompt[4], 'Context: labels, connected and nearby elements:');
    assert.match(prompt[5], /"id":"label-a"/);
    assert.throws(() => captureContext(doc, {selection:['missing']}), /no longer available/);
  } finally { doc.destroy(); }
  const large = await handle({action:'create',id:'large',opId:'create',actor:'Alice',kind:'document',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'x'.repeat(140000)}]}]}});
  const largeDoc = load(large.state);
  try {
    // Nothing is too large to ask about: past the budget the model reads on.
    const whole = captureContext(largeDoc);
    const {result} = await handle({action:'read',state:large.state,question:true,expected:contentHash(whole.snapshot)});
    assert.ok(result.prompt.length <= 128 * 1024);
    assert.match(result.prompt, /^document "Document" at shared:\/\/large, revision 1; the question is about the whole document\./);
    assert.match(result.prompt, /1 more line omitted; Read shared:\/\/large for the rest\.$/);
    const text = largeDoc.getXmlFragment('document').get(0).get(0);
    assert.equal(captureContext(largeDoc, {range:rangeFor(text,0,text,3)}).quote, 'xxx');
  } finally { largeDoc.destroy(); }
});

test('discussion replies are attributed, retry-safe and asked about at the reviewed version', async () => {
  let {state} = await handle({action:'create',id:'doc',opId:'create',actor:'Alice',kind:'document',
    content:{type:'doc',content:[{type:'paragraph',attrs:{id:'a'},content:[{type:'text',text:'A data model.'}]}]}});
  const doc = load(state);
  try {
    const text = doc.getXmlFragment('document').get(0).get(0);
    const range = rangeFor(text,2,text,12), expected = contentHash(captureContext(doc,{range}).snapshot);
    ({state} = await handle({action:'discussion-post',state,opId:'first',messageId:'first',discussionId:'thread',
      actor:'Alice',target:{kind:'passage',range},expected,text:'What does this mean?'}));
    const reply = {action:'discussion-reply',opId:'reply',messageId:'reply',actor:'Bob',discussionId:'thread',
      text:'Please include an example.'};
    ({state} = await handle({...reply,state}));
    assert.deepEqual(state.discussions[0].messages.map(m => [m.id, m.actor]), [['first','Alice'],['reply','Bob']]);
    assert.equal((await handle({...reply,state})).state.discussions[0].messages.length,2);
    const ask = {action:'read',state,question:true,discussionId:'thread',version:'reply',range,expected};
    const {result} = await handle(ask);
    // Emacs adds the discussion; the host describes the content.
    assert.equal(result.snapshot.discussion, undefined);
    assert.match(result.prompt, /the question is about a selection\.\nSelected text:/);
    await assert.rejects(handle({...ask,version:'first'}),/Discussion changed/);
    await assert.rejects(handle({...ask,discussionId:'missing'}),/no longer available/);
    await assert.rejects(handle({...reply,state,opId:'empty',messageId:'empty',text:' '}),/message is required/);
    await assert.rejects(handle({...reply,state,opId:'unknown',messageId:'unknown',discussionId:'missing'}),/no longer available/);
    ({state} = await handle({action:'discussion-resolve',state,opId:'resolve',actor:'Bob',discussionId:'thread',resolved:true}));
    await assert.rejects(handle({...reply,state,opId:'closed',messageId:'closed'}),/Reopen/);
    await assert.rejects(handle({...ask,state,version:'reply'}),/Reopen/);
    ({state} = await handle({action:'discussion-resolve',state,opId:'reopen',actor:'Bob',discussionId:'thread',resolved:false}));
    ({state} = await handle({...reply,state,opId:'reply-two',messageId:'reply-two',text:'And a diagram?'}));
    assert.equal(state.discussions[0].messages.length,3);
    ({state} = await handle({action:'discussion-answered',state,opId:'answered',actor:'Alice',discussionId:'thread',
      session:'s1',sessionName:'Design chat'}));
    assert.deepEqual([state.discussions[0].session, state.discussions[0].sessionName], ['s1','Design chat']);
    const exported = await handle({action:'export',format:'native',state});
    assert.doesNotMatch(exported.result.text,/Please include|And a diagram/);
  } finally { doc.destroy(); }
});

test('the general discussion collects whole-item messages; a selection may be sent alone', async () => {
  let {state} = await handle({action:'create',id:'board',opId:'create',actor:'Alice',kind:'whiteboard',
    content:[...shape('a', 0, 'A')]});
  const post = (fields) => handle({action:'discussion-post',actor:'Alice',state,...fields});
  for (const id of ['g1','g2'])
    ({state} = await post({opId:id,messageId:id,target:{kind:'whole'},text:`Message ${id}`}));
  assert.equal(state.discussions.length, 1);
  assert.equal(state.discussions[0].id, 'general');
  assert.deepEqual(state.discussions[0].target, {kind:'whole'});
  assert.deepEqual(state.discussions[0].messages.map(m => m.id), ['g1','g2']);
  // A message about the whole item needs words, and it stays open.
  await assert.rejects(post({opId:'g3',messageId:'g3',target:{kind:'whole'},text:'',alone:true}), /Only a selection/);
  await assert.rejects(post({opId:'g4',messageId:'g4',target:{kind:'whole'},text:' '}), /message is required/);
  await assert.rejects(handle({action:'discussion-resolve',state,opId:'r',actor:'Alice',discussionId:'general',
    resolved:true}), /stays open/);
  // Asking about the general discussion sends the whole item.
  const asked = await handle({action:'read',state,question:true,discussionId:'general',version:'g2',whole:true});
  assert.equal(asked.result.snapshot.scope, 'whole');
  await assert.rejects(handle({action:'read',state,question:true,discussionId:'general',version:'g2',
    selection:['a']}), /no longer available/);
  // A selection sent alone carries no text, and only alone may it be empty.
  const doc = load(state);
  const expected = contentHash(captureContext(doc, {selection:['a']}).snapshot);
  doc.destroy();
  ({state} = await post({opId:'s1',messageId:'s1',discussionId:'alone',target:{kind:'objects',selection:['a']},
    text:'',alone:true,expected}));
  const alone = state.discussions.find(d => d.id === 'alone');
  assert.deepEqual(alone.messages.map(m => [m.text, m.alone]), [['', true]]);
  assert.equal(alone.target.kind, 'objects');
  await assert.rejects(post({opId:'s2',messageId:'s2',discussionId:'words',target:{kind:'objects',selection:['a']},
    text:'Draw it',alone:true,expected}), /message is required/);
  // The assistant resolves a selection sent alone only while nobody wrote in it.
  const settle = (from, opId) => handle({action:'discussion-resolve',state:from,opId,actor:'Assistant',
    discussionId:'alone',resolved:true,ifAlone:true});
  const done = (await settle(state, 'settle-1')).state;
  assert.deepEqual([done.discussions.find(d => d.id === 'alone').resolved,
    done.discussions.find(d => d.id === 'alone').resolvedBy], [true, 'Assistant']);
  ({state} = await handle({action:'discussion-reply',state,opId:'n1',actor:'Bob',discussionId:'alone',
    messageId:'n1',text:'Keep the legend'}));
  const kept = (await settle(state, 'settle-2')).state ?? state;
  assert.equal(kept.discussions.find(d => d.id === 'alone').resolved, false);
  // Comments of the earlier mechanism go with the next commit.
  const legacy = {...state, comments:[{id:'old',text:'Old',selection:['a']}]};
  const next = await post({state:legacy,opId:'g5',messageId:'g5',target:{kind:'whole'},text:'Next'});
  assert.equal(next.state.comments, undefined);
  assert.equal((await handle({action:'read',state:legacy})).result.comments, undefined);
});

test('board areas carry their region, nearby objects without image bytes and a cropped PNG', async () => {
  const image = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=';
  const {state} = await handle({action:'import',format:'excalidraw',id:'board',opId:'create',actor:'Alice',
    data:JSON.stringify({type:'excalidraw',version:2,elements:[
      ...shape('a', 0, 'A'), ...shape('far', 2000, 'Far', 'ellipse', 2000),
      {id:'pic',type:'image',x:150,y:0,width:100,height:100,fileId:'pixel'}],
      files:{pixel:{id:'pixel',mimeType:'image/png',dataURL:image,created:1}}})});
  const doc = load(state);
  const size = png => { const b = Buffer.from(png, 'base64'); return [b.readUInt32BE(16), b.readUInt32BE(20)]; };
  try {
    const region = [-10,-10,200,120];
    const captured = captureContext(doc, {selection:['a'], region});
    assert.deepEqual(captured.snapshot.region, region);
    assert.equal(captured.snapshot.scope, 'selection');
    assert.deepEqual(captured.snapshot.content.map(s => s.id), ['a']);
    assert.deepEqual(captured.snapshot.context.map(s => s.id), ['label-a', 'pic'], 'touched neighbours, not distant objects');
    assert.equal(captured.snapshot.context[1].fileId, 'pixel');
    assert.doesNotMatch(JSON.stringify(captured.snapshot), /base64/, 'image bytes stay out of the question');
    assert.match(captured.quote, /^Area 200 × 120 at -10, -10\n1 object\nrectangle: A$/);
    const empty = captureContext(doc, {region:[500,500,50,40]});
    assert.deepEqual([empty.snapshot.content, empty.snapshot.context], [[], []]);
    assert.match(empty.quote, /^Area 50 × 40 at 500, 500\n0 objects$/);
    for (const bad of [[0,0,0,10], [0.5,0,10,10], [0,0,10], 'area'])
      assert.throws(() => captureContext(doc, {region:bad}), /Invalid board area/);
    const {result} = await handle({action:'read',state,question:true,selection:['a'],region,
      expected:contentHash(captured.snapshot),image:true,imageMax:1024});
    assert.deepEqual(result.snapshot.region, region);
    assert.equal(result.prompt.split('\n')[1], `Area: x ${region[0]}, y ${region[1]}, ${region[2]} × ${region[3]}`);
    assert.deepEqual(size(result.png), [992, 672], 'area plus margin, upscaled to stay legible');
    const whole = await handle({action:'read',state,question:true,selection:['a'],
      expected:contentHash(captureContext(doc, {selection:['a']}).snapshot),image:true});
    assert.deepEqual(size(whole.result.png), [160, 160], 'object questions keep their own framing');
    await assert.rejects(handle({action:'read',state,question:true,selection:['a'],region:[0,0,10,10],
      expected:contentHash(captured.snapshot)}), /Content changed/);
  } finally { doc.destroy(); }
});

test('board discussions anchor to objects and areas, track changes and send surviving objects', async () => {
  let {state} = await handle({action:'create',id:'board',opId:'create',actor:'Alice',kind:'whiteboard',content:[
    ...shape('a', 0, 'A'), ...shape('b', 300, 'B')]});
  const commit = async (edit) => {
    const doc = load(state);
    try {
      edit(doc.getMap('elements'));
      ({state} = await handle({action:'update',opId:crypto.randomUUID(),actor:'Guest: Bob',state,
        update:Buffer.from(Y.encodeStateAsUpdate(doc)).toString('base64')}));
    } finally { doc.destroy(); }
  };
  const statuses = () => { const doc = load(state); try { return readDiscussions(doc, state.discussions); } finally { doc.destroy(); } };
  let doc = load(state);
  const objects = contentHash(captureContext(doc, {selection:['a','b']}).snapshot);
  const area = contentHash(captureContext(doc, {region:[500,0,80,60]}).snapshot);
  doc.destroy();
  const post = (fields) => handle({action:'discussion-post',state,actor:'Alice',...fields});
  ({state} = await post({opId:'objects',messageId:'objects',discussionId:'objects',text:'Align these',
    target:{kind:'objects',selection:['a','b']},expected:objects}));
  ({state} = await post({opId:'area',messageId:'area',discussionId:'area',text:'Legend here',
    target:{kind:'objects',selection:[],region:[500,0,80,60]},expected:area}));
  await assert.rejects(post({opId:'none',messageId:'none',discussionId:'none',text:'?',
    target:{kind:'objects',selection:[]},expected:objects}), /Select objects or an area/);
  assert.deepEqual(state.discussions[0].target.selection, ['a','b']);
  assert.match(state.discussions[0].target.signature, /^[0-9a-f]{16}$/);
  assert.equal(state.discussions[0].quote, '2 objects\nrectangle: A\nrectangle: B');
  assert.deepEqual(state.discussions[1].target.region, [500,0,80,60]);
  assert.deepEqual(statuses().map(c => c.anchorStatus), ['current','current']);
  await commit(shapes => shapes.get('label-b').set('text', 'B2'));
  assert.deepEqual(statuses().map(c => c.anchorStatus), ['changed','current'], 'an edited label changes its discussion');
  await commit(shapes => shapes.delete('b'));
  const [thread] = statuses();
  assert.equal(thread.anchorStatus, 'changed');
  assert.deepEqual(thread.liveSelection, ['a']);
  assert.equal(thread.liveQuote, '1 object\nrectangle: A');
  doc = load(state);
  const surviving = contentHash(captureContext(doc, {selection:thread.liveSelection}).snapshot);
  doc.destroy();
  const ask = {action:'read',state,question:true,discussionId:'objects',version:'objects',
    selection:['a'],expected:surviving};
  assert.equal((await handle(ask)).result.snapshot.scope, 'selection');
  await assert.rejects(handle({...ask,selection:['a','intruder']}), /no longer available/);
  await assert.rejects(handle({...ask,discussionId:'area',version:'area',region:[0,0,10,10]}), /no longer available/);
  await commit(shapes => shapes.delete('a'));
  assert.deepEqual(statuses().map(c => c.anchorStatus), ['deleted','current'], 'an area outlives its objects');
});

test('room questions about a whole item capture current content without a reviewed snapshot', async () => {
  let {state} = await handle({action:'create',id:'board',opId:'create',actor:'Guest: Alice',kind:'whiteboard',
    content:[...shape('a', 0, 'A'), ...shape('b', 200, 'B', 'ellipse')]});
  const asked = await handle({action:'read',question:true,whole:true,state});
  assert.equal(asked.result.snapshot.scope, 'whole');
  assert.deepEqual(asked.result.snapshot.content.map(shape => shape.id), ['a','b','label-a','label-b']);
  assert.match(asked.result.quote, /^2 objects/);
  // Without the whole-item flag a question still needs the snapshot its
  // sender reviewed, and the flag never combines with a selection.
  await assert.rejects(handle({action:'read',question:true,state}), /Content changed/);
  await assert.rejects(handle({action:'read',question:true,whole:true,selection:['a'],state}),
                       /whole-item question takes no selection/);
});

test('a board quote names labelled objects and counts the rest by type', async () => {
  const stroke = (id, x) => ({id, type:'freedraw', x, y:0, width:10, height:10, points:[[0,0],[10,10]]});
  const {state} = await handle({action:'create',id:'strokes',opId:'create',actor:'Alice',kind:'whiteboard',content:[
    ...shape('a', 0, 'A'), stroke('s1', 200), stroke('s2', 300), ...shape('b', 400), stroke('s3', 500)]});
  const doc = load(state);
  try {
    assert.equal(captureContext(doc, {selection:['a','s1','s2','b','s3']}).quote,
      '5 objects\nrectangle: A\nrectangle\nfreedraw ×3');
  } finally { doc.destroy(); }
});
