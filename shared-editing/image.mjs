/* Embedded image validation shared by documents and whiteboards. */
function check(ok, message) {
  if (!ok) throw new Error(message);
}
export const imageSource = image => image.imageEdit?.src || image.src;
export function validateImageContent(image) {
  let pixels = validateImage(image.src);
  const edit = image.imageEdit;
  if (edit == null) return pixels;
  check(edit && typeof edit === 'object' && !Array.isArray(edit) &&
    Object.keys(edit).every(key => ['src', 'crop', 'rotation', 'flipX', 'flipY'].includes(key)),
    'Invalid image edit');
  const c = edit.crop;
  check(Array.isArray(c) && c.length === 4 && c.every(Number.isFinite) &&
    c[0] >= 0 && c[1] >= 0 && c[2] > 0 && c[3] > 0 &&
    c[0] + c[2] <= 1.00000001 && c[1] + c[3] <= 1.00000001,
    'Invalid image crop');
  check([0, 90, 180, 270].includes(edit.rotation) &&
    typeof edit.flipX === 'boolean' && typeof edit.flipY === 'boolean', 'Invalid image orientation');
  pixels += validateImage(edit.src);
  return pixels;
}
export function validateImage(src) {
  const match = /^data:image\/(png|jpeg|webp);base64,([A-Za-z0-9+/]*={0,2})$/.exec(src);
  check(match && src.length <= 6 * 1024 * 1024, 'Invalid embedded image');
  const raw = atob(match[2]),
    data = Uint8Array.from(raw, (c) => c.charCodeAt(0)),
    v = new DataView(data.buffer);
  let width = 0,
    height = 0;
  if (
    match[1] === 'png' &&
    data.length >= 33 &&
    v.getUint32(0) === 0x89504e47 &&
    v.getUint32(4) === 0x0d0a1a0a &&
    v.getUint32(8) === 13 &&
    v.getUint32(12) === 0x49484452
  ) {
    width = v.getUint32(16);
    height = v.getUint32(20);
    let pixels = false,
      ended = false;
    for (let offset = 8; offset < data.length;) {
      check(offset + 12 <= data.length, 'Truncated PNG');
      const size = v.getUint32(offset),
        kind = raw.slice(offset + 4, offset + 8);
      check(offset + 12 + size <= data.length, 'Truncated PNG');
      check(kind !== 'acTL', 'Animated images are unsupported');
      if (kind === 'IDAT') pixels = true;
      if (kind === 'IEND') {
        check(size === 0 && offset + 12 === data.length, 'Malformed PNG ending');
        ended = true;
      }
      offset += 12 + size;
    }
    check(pixels && ended, 'Truncated PNG');
  } else if (match[1] === 'jpeg' && data.length >= 4 && v.getUint16(0) === 0xffd8) {
    check(v.getUint16(data.length - 2) === 0xffd9, 'Truncated JPEG');
    for (let offset = 2; offset + 4 <= data.length;) {
      check(data[offset] === 255, 'Malformed JPEG');
      const marker = data[offset + 1],
        length = v.getUint16(offset + 2);
      check(length >= 2 && offset + 2 + length <= data.length, 'Truncated JPEG');
      if ([0xc0, 0xc1, 0xc2].includes(marker)) {
        check(length >= 8, 'Malformed JPEG dimensions');
        height = v.getUint16(offset + 5);
        width = v.getUint16(offset + 7);
        break;
      }
      if (marker === 0xda || marker === 0xd9) break;
      offset += 2 + length;
    }
  } else if (
    match[1] === 'webp' &&
    data.length >= 30 &&
    raw.slice(0, 4) === 'RIFF' &&
    raw.slice(8, 12) === 'WEBP'
  ) {
    check(v.getUint32(4, true) === data.length - 8, 'Truncated WebP');
    const kind = raw.slice(12, 16),
      u24 = (i) => data[i] + (data[i + 1] << 8) + (data[i + 2] << 16);
    if (kind === 'VP8X') {
      check(!(data[20] & 2), 'Animated images are unsupported');
      width = 1 + u24(24);
      height = 1 + u24(27);
    } else if (kind === 'VP8 ' && data[23] === 0x9d && data[24] === 1 && data[25] === 0x2a) {
      width = v.getUint16(26, true) & 0x3fff;
      height = v.getUint16(28, true) & 0x3fff;
    } else if (kind === 'VP8L' && data[20] === 0x2f) {
      const bits = v.getUint32(21, true);
      width = 1 + (bits & 0x3fff);
      height = 1 + ((bits >>> 14) & 0x3fff);
    }
  }
  check(
    width > 0 && height > 0 && width <= 8192 && height <= 8192 && width * height <= 16000000,
    'Image dimensions are unsupported or exceed 16 megapixels',
  );
  return width * height;
}
