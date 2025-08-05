// This script cleans binary ninja assembly output to uiCA asm format

import readline from 'readline';

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

const lines: string[] = [];

rl.on('line', (line) => {
    const finalParts: string[] = [];

    const parts = line.split(/[ ,]+/);
    let startIndex = parts[0].length;

    if (!/^[0-9a-fA-F]+$/.test(parts[0])) {
        startIndex = 0;
    }

    const formatted = line
        .replace(/{.*}/g, "")
        .replace(/, k[0-9],/g, ",")

    console.log(formatted.substring(startIndex).trim());
});
