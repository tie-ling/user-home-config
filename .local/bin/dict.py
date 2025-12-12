#!/usr/bin/env python
# https://ftp.halifax.rwth-aachen.de/aarddict/dewiki/dewiktionary20250701-slob/

from __future__ import annotations

from prompt_toolkit.shortcuts import choice
from prompt_toolkit import prompt

import slob
import subprocess
import sys
import os

# 0. ask for a word
# 1. generate a list of tuple of matches
# 2. prompt in prompt toolkit
# 3. render result in w3m

def find_word():
    query = prompt('?: ')
    found_words = []
    with slob.open('/home/yc/Documents/dict/dewiktionary.slob') as s:
        for i, item in enumerate(slob.find(query, s, match_prefix=True)):
            _, blob = item
            found_words.append((blob.id, blob.key))
            if i == 10:
                break
    return found_words

def show_results(found_words) -> None:
    blob_id = choice(
        message="Select a result:",
        options=found_words,
        mouse_support=True,
    )
    with slob.open('/home/yc/Documents/dict/dewiktionary.slob') as s:
        _content_type, content = s.get(blob_id)
        subprocess.run(["w3m", "-T", "text/html", "-o", "confirm_qq=0"], 
                       input=content)

if __name__ == "__main__":
    try:
        while True:
            found_words=find_word()
            show_results(found_words)
    except KeyboardInterrupt:
        print('Interrupted')
        try:
            sys.exit(130)
        except SystemExit:
            os._exit(130)

