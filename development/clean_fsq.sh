# Previamente se eliminan los urls de cada tweet. Cambiamos los delimitadores de campo. 
# Eliminamos caracteres mal codificados.

sed 1d fsq.csv > fsq2.csv
rm fsq.csv
mv fsq2.csv fsq.csv

gawk 'BEGIN{ FS = "|" }; {if ($3 ~ /@/) print }' fsq.csv > fsq_at.csv
gawk 'BEGIN{ FS = "|" }; {if ($3 ~ /m at/) print }' fsq.csv > fsq_im.csv

cut -d'(' -f1 fsq_im.csv > fsq_im_tmp.csv

# Quitamos el "I'm at" y agregamos el delimitador de texto al final de cada línea.

rm fsq_im.csv
mv fsq_im_tmp.csv fsq_im.csv

cut -d'@' -f2 fsq_at.csv | cut -d'(' -f1 | cut -d')' -f1 > fsq_at_tmp.csv

# cambiamos w\ con un delimitador pipe para quedarnos con lo que nos interesa

cut -d'|' -f1 fsq_at_tmp.csv > fsq_at_tmp2.csv
rm fsq_at_tmp.csv

cut -d'|' -f1,2 fsq_at.csv > fsq_at_tmp3.csv
rm fsq_at.csv

paste -d '|' fsq_at_tmp3.csv fsq_at_tmp2.csv > fsq_at.csv
rm fsq_at_tmp*

cat fsq_at.csv fsq_im.csv > fsq_clean.csv
