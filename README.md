# Emoji Translator

Analyzes a passage of text and returns the most relevant emoji.

Created as a project for COSC-255 "Programming: Haskell".

## Algorithm
An emoji's relevance to the input text is calculated by comparing the words in the input text to the [annotations assigned to emoji](http://unicode.org/emoji/charts/emoji-annotations.html) by the Unicode Consortium. Each time a word (or an inflected form of the word) in the input text or one of its synonyms matches an annotation (or an inflected form of the annotation) or one of its synonyms, 1/(2^(_n_-_m_+1)*2^_s_) points are added to the score of each of the emoji connected to that annotation, where _n_ represents the [frequency class](http://self.gutenberg.org/articles/frequency_list) of the word in the input text, _m_ represents the frequency class of the least-common word, and _s_ represents how many thesaurus lookups were necessary to make that match.

For example:
- _s_=0 indicates that the word (or one of its inflections) exactly matches the annotation (or one of its inflections).
- _s_=1 indicates that either:
    - a **synonym** of the word (or one of its inflections) exactly matches the annotation (or one of its inflections).
    - the word (or one of its inflections) exactly matches a **synonym** the annotation (or one of its inflections).
- _s_=2 indicates that a **synonym** of the word (or one of its inflections) exactly matches a **synonym** the annotation (or one of its inflections).

By default, a maximum of 1 thesaurus lookup for each word/annotation is permitted. (The maximum value of _s_ is therefore 2.)

Please note that at the time of writing this code, I had not yet taken a course in computational linguistics. Therefore, this algorithm is very rudimentary and ill-advised.

## Input files

The following files are required in current working directory. I have not included them due to copyright concerns.
- Emoji annotations
    - A language-specific XML file of emoji annotations from [the Unicode CLDR](http://unicode.org/repos/cldr/tags/latest/common/annotations/)
- Thesaurus
    - Gary Ward's [Moby Thesaurus](http://icon.shef.ac.uk/Moby/mthes.html) **OR**
    - A thesaurus with entries separated by newlines. Entries should be in the following format:
        - *word*,*synonym*,*synonym*,*snyonym*,...
- Word-frequency list
    - count_1w.txt from [Peter Norvig](http://norvig.com/ngrams/) **OR**
    - A word-frequency list with entries separated by newlines. Entries should be in the following format:
        - *word*\t*count*
- A list of word inflections
    - 2of12id.txt from the [Unofficial Alternate 12Dicts package](http://wordlist.aspell.net/12dicts/) **OR**
    - A list of word inflections with entries separated by newlines. Entries should be in the following format:
        - *word*: *inflection*, *inflection*
- Unicode character names
    - [allkeys_CLDR.txt](http://unicode.org/repos/cldr/tags/latest/common/uca/allkeys_CLDR.txt) from the Unicode CLDR

## Running

You will need to install certain modules via *cabal install*; they are noted in the imports section of the code.

Provide the input text via stdin.

For whatever reason, this program takes up multiple gigabytes of RAM. By default, Haskell assigns compiled programs ~8MB of stack space. For this reason, the program will need to be run via *runhaskell* or *ghci*.

## Output

The program outputs all emoji with a score greater than 0 (with their names and scores alongside) sorted ascendingly by score.

## To-do

I don't actually have the time to implement any of these changes, but I would if I could!

- Figure out why the program takes up multiple gigabytes of RAM, and reduce its memory impact.
- Tweak to formulas to produce more relevant results.
- Reduce score-inflation by synonymous annotations.

## License

Copyright Jeremiah Megel 2015

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

