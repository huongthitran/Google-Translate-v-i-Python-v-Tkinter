import googletrans
#print(googletrans.LANGUAGES)
from googletrans import Translator # Translator se giup cho minh dich ngon ngu
t = Translator() # Tao mot object cua class Translator - Tao object de goi phuong thuc
a = t.translate("em dep qua", src ="vi", dst ="en") # dung t goi phuong thuc translate
b = a.text
print(b)

