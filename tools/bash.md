Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or
equivalents) available. Use this to inspect system state, run builds,
tests or other development or system administration tasks.

IMPORTANT: Do NOT use this for file operations, finding, reading or
editing files. Use the provided file tools instead: `Read`, `Write`,
`Edit`, `Glob`, `Grep`.

# Instructions

- Commands run from the project root by default
- Quote file paths with spaces using double quotes
- Chain dependent commands with `&&` (or `;` if failures are OK)
- Use absolute paths instead of `cd` when possible
- For parallel commands, make multiple `Bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make
- Do NOT use newlines to separate commands (newlines are ok in quoted strings)

### When to use `Bash`

- System commands: git, make, compiler commands, etc.
- Commands that truly require shell execution
- Running tests or builds

### When NOT to use `Bash`

- File operations -> use dedicated file tools instead
- Finding files -> use `Glob`
- Searching contents -> use `Grep`
- Reading files -> use `Read`
- Editing files -> use `Edit`
- Writing files -> use `Write`
- Communication with user -> output text directly

### Examples of good usage

<example>
- Building the project:
Bash(command="make build && make test")
</example>

<example>
- Checking git status:
Bash(command="git status")
</example>

### Examples of bad usage

<example>
- Using echo for communication:
Bash(command="echo 'Processing complete'")
<reasoning>
Should output text directly instead of using bash echo.
</reasoning>
</example>

<example>
- Reading file contents:
Bash(command="cat config.yml")
<reasoning>
Should use Read tool instead for better integration.
</reasoning>
</example>
