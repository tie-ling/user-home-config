#!/usr/bin/env python
# https://ftp.halifax.rwth-aachen.de/aarddict/dewiki/dewiktionary20250701-slob/

import argparse
import slob
from pathlib import Path
import sys
import os
import subprocess

# 0. take all arguments as query string
# 1. find identical result
# 2. render identical result

from icu import Locale, Collator, UCollAttribute, UCollAttributeValue

def find_identical(word, slobs):
    seen = set()
    slobs = [slobs]

    variants = []

    variants.append((Collator.IDENTICAL, None))

    for strength, maxlength in variants:
        for slob in slobs:
            d = slob.as_dict(strength=strength, maxlength=maxlength)
            for item in d[word]:
                dedup_key = (slob.id, item.id, item.fragment)
                if dedup_key in seen:
                    continue
                else:
                    seen.add(dedup_key)
                    yield slob, item

if __name__ == "__main__":
    # parse command arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("--source",
                        type = Path,
                        default='~/Documents/dict/dewiktionary.slob')
    parser.add_argument("--query",
                        required=True)
    args = parser.parse_args()

    # open dictionary
    s = slob.open(os.fspath(args.source))

    # return blob id from the first result
    for i, item in enumerate(find_identical(args.query, s)):
        _, blob = item
        if i == 1:
            break

    if 'blob' not in globals():
        print("word not found")
        sys.exit()

    # retrieve content
    _content_type, content = s.get(blob.id)

    # render content
    rendered = subprocess.run(["w3m", "-T", "text/html", "-dump"], input=content.decode('utf-8'), capture_output=True, text=True)

    # close dictionary
    s.close()

    print(rendered.stdout)
    sys.exit()
