
word='ours'

invalid_words=['they','hers','theirs','them'] # assume this contains a list of invalid words 

def is_valid_word(word,list):
    # Input: w‐> a word
    # Output: True if the word is valid, False otherwise
    # A word is valid if it has more than 2 letters, and is not in the "invalid_words" list
    if len(word)>2:
        print(word not in invalid_words)

is_valid_word(word,invalid_words)

###########################################################

def get_words_from_sentence(text):
    return text.split()

get_words_from_sentence('Hello world my name is Python')