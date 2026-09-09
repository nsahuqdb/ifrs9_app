#!/usr/bin/env python3
"""Strict R syntax sanity checker: string/comment-aware bracket balance +
unicode-bracket detection. Usage: python3 rcheck.py <file.R>"""
import sys

def check(path):
    src = open(path, encoding='utf-8').read()
    stack = []
    line = 1
    i = 0
    n = len(src)
    state = None  # None | "'" | '"' | '%' (percent-op) | backtick
    problems = []
    OPEN = {'(': ')', '[': ']', '{': '}'}
    CLOSE = {')': '(', ']': '[', '}': '{'}
    UNI = set('（）［］｛｝「」『』〔〕【】')
    while i < n:
        c = src[i]
        if c == '\n':
            line += 1
            if state == '#':
                state = None
            i += 1
            continue
        if state == '#':
            i += 1
            continue
        if state in ("'", '"', '`'):
            if c == '\\' and state != '`':
                i += 2
                continue
            if c == state:
                state = None
            i += 1
            continue
        # not in string/comment
        if c == '#':
            state = '#'
        elif c in ("'", '"', '`'):
            state = c
        elif c in OPEN:
            stack.append((c, line))
        elif c in CLOSE:
            if not stack:
                problems.append(f"line {line}: unmatched closing '{c}'")
            else:
                o, ol = stack.pop()
                if OPEN[o] != c:
                    problems.append(
                        f"line {line}: '{c}' closes '{o}' opened line {ol}")
        elif c in UNI:
            problems.append(f"line {line}: full-width unicode bracket {c!r}")
        i += 1
    for o, ol in stack:
        problems.append(f"unclosed '{o}' opened line {ol}")
    if state in ("'", '"', '`'):
        problems.append(f"unterminated string ({state}) at EOF")
    if problems:
        print(f"PROBLEMS in {path}:")
        for p in problems[:20]:
            print("  " + p)
        return 1
    print(f"OK: {path} balanced, no unicode brackets")
    return 0

if __name__ == '__main__':
    sys.exit(max(check(p) for p in sys.argv[1:]))
