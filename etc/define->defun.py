import re, sys

# Might be useful keeping it around

def convert(text):
    return re.sub(
        r'\(define \(([a-zA-Z0-9_?!/<>*+=-]+)((?:\s+[^\)]+)*)\)',
        lambda m: f'(defun {m.group(1)} ({m.group(2).strip()})',
        text
    )

for path in sys.argv[1:]:
    with open(path) as f: text = f.read()
    with open(path, 'w') as f: f.write(convert(text))
