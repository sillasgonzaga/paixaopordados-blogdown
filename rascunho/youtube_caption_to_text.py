#import pandas
# youtube-dl -i -v -w --skip-download --write-auto-sub --sub-lang pt https://www.youtube.com/channel/UC8mDF5mWNGE-Kpfcvnn0bUg
from webvtt import WebVTT

def caption_to_vector(file):
  x = WebVTT().read(file)
  txt = [caption.text for caption in x]
  return(txt)


#webvtt = WebVTT().read("/home/sillas/Downloads/mepoupe/3 CASOS TERRÍVEIS DO GUSTAVO CERBASI_ O papa das finanças (e como ele resolveu)-_1wm8qYwHmk.pt.vtt")
#webvtt.caption
#webvtt.save_as_srt("a.srt")

# for caption in x
#     print(caption.text)

