Preliminary experiments with "vibe coding" LE programs from informal requirements

**Please DO NOT use any of the .le files in this directory other than for experimentation!**

See also CLAUDE.md elsewhere in this repo, and launch Claude Code with

    claude --append-system-prompt "You are an expert converting regulatory texts into Logic English programs"

Example prompts:

    convert @vibing/SimplifiedAgreement.txt into a new LE program

    fetch web page https://www.refworld.org/legal/legislation/natlegbod/1981/en/17879, and convert its Part I (sections 1-14) into a new LE program

After Claude Code is done, it's better to manually check that the program is doing what Claude tells us, by executing the LE verify command: 

    swipl le_command.pl --command=verify <myProgram.le>



To look at explanations, paste the program into a public SWISH LE window, or use the simple server form:

	swipl simple_server.pl --port=3052 --staticdir='./build'

..and browse:

    http://localhost:3052/static/simple_client.html

	operation: 'explain' or 'answer'
	paste text into central text field
    Paste query and scenario names into right fields
    Click "Answer" button
