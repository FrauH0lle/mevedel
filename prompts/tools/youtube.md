Fetch YouTube video description and transcript.

Returns markdown formatted string with two sections:
- "description": Video description added by uploader
- "transcript": Video transcript in SRT format (timestamped)

### When to use `YouTube`

- Extracting content from YouTube tutorial videos
- Getting transcripts of conference talks or presentations
- Reading video descriptions for context or links
- Analyzing spoken content from educational videos

### When NOT to use `YouTube`

- Non-YouTube video platforms -> use `WebFetch` if available
- Videos without transcripts (will fail or return empty)
- Looking for video URLs -> use `WebSearch` first

### How to use `YouTube`

- Requires full YouTube URL
- URL format: "https://www.youtube.com/watch?v=VIDEO_ID"
- Transcript returned in SRT format with timestamps
- May fail if video has no transcript/captions available

### Examples of good usage

<example>
- Get tutorial content:
YouTube(url="https://www.youtube.com/watch?v=H2qJRnV8ZGA")
</example>

### Examples of bad usage

<example>
- Using partial URL:
YouTube(url="youtube.com/watch?v=ABC123")
<reasoning>Need full URL with protocol: https://</reasoning>
</example>
