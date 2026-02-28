// This script cleans binary ninja assembly output to uiCA asm format

import fs from 'fs';

fs.readFile('input.asm', 'utf-8', (err, data) => {
    if (err) {
        console.error('Error reading file:', err);
        return;
    }

    const lines = data.split('\n');

    lines.forEach(line => {
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
});
