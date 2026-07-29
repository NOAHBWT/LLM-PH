import re
from collections import Counter
import matplotlib.pyplot as plt
import spacy
from wordcloud import WordCloud


# Set Stopwords
built_in_stopwords = nlp.Defaults.stop_words

custom_stopwords = {
    "study",
    "result",
    "method",
    "data",
    "analysis",
    "finding",
    "report",
    "evaluate",
    "show",
    "assess",
    "observe",
    "suggest",
    "patient",
    "subject",
    "sample",
    "group",
    "year",
    "total",
    "level",
    "value",
    "high",
    "low",
    "significant",
}

FINAL_STOPWORDS = set(built_in_stopwords).union(custom_stopwords)



# Cleaning -> Word Segmentation -> Part-of-Speech Tagging -> Morphological Restoration -> Phrase Extraction -> Double Filtering
def preprocess_text_for_wordcloud(text, stopwords_set):
    clean_text = re.sub(r"[^a-zA-Z\s-]", " ", text)
  
    doc = nlp(clean_text)

    meaningful_terms = []

    for chunk in doc.noun_chunks:
        phrase_lemmas = []

        for token in chunk:

            lemma = token.lemma_.lower().strip()

            if (
                token.pos_ in ["NOUN", "PROPN", "ADJ"]
                and lemma not in stopwords_set
                and len(lemma) > 2
            ):
                phrase_lemmas.append(lemma)

        full_phrase = " ".join(phrase_lemmas).strip()

        if full_phrase and full_phrase not in stopwords_set:
            meaningful_terms.append(full_phrase)

    return meaningful_terms


raw_text = """
inference process
"""

extracted_terms = preprocess_text_for_wordcloud(raw_text, FINAL_STOPWORDS)

# Count of frequencies
term_counts = Counter(extracted_terms)

# Visualizing Wordcloud

wordcloud = WordCloud(
    width=1000,
    height=500,
    background_color="white",
    stopwords=FINAL_STOPWORDS,
    collocations=False, 
    min_font_size=12,
    max_words=50,
).generate_from_frequencies(term_counts)

plt.figure(figsize=(10, 5))
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.title("Academic Word Cloud (Tokenized & Lemmatized)", fontsize=12)
plt.tight_layout()
plt.show()
