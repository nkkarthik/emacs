
export function lineAt(body: string, offset: number): string {
    if (!body || offset > body.length) {
        return "";
    }

    let start = offset;
    let end = offset + 1;

    // If offset is at newline or at the end, step back
    if (offset === body.length || (body[start] === '\n' && start > 0)) {
        start--;
        end--;
    }

    // Go back until previous unescaped newline
    while (start > 0) {
        if (body[start] === '\n') {
            if (start === 0 || body[start - 1] !== '\\') {
                start++;
                break;
            }
        }
        start--;
    }

    // Go forward until next unescaped newline
    while (end < body.length) {
        if (body[end] === '\n') {
            if (end === 0 || body[end - 1] !== '\\') {
                break;
            }
        }
        end++;
    }

    return body.slice(start, end);
}
