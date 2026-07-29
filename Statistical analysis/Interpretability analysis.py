import nltk
from nltk.corpus import stopwords

stop_words = set(stopwords.words('english'))

# 针对你研究领域的自定义停用词（如论文常见的通用词：study, result, method等）
custom_stopwords = {"study", "result", "analysis", "data", "method", "model", "show"}
all_stopwords = stop_words.union(custom_stopwords)

