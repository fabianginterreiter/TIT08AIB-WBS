(print '("EinZimmer" "EG" "Gas-Zentral" "ja" "nah" "fern" "erreichbar" "nein" "301-350" "101-150" "31-50Jahre" "ja" "Wohngebiet" "Zentrum" "1000-1500" "K�che(alt)" "Dusche" "ja" "ja" "ja" "teilm�bliert" "41-50"))
(print (is-element-good '("EinZimmer" "EG" "Gas-Zentral" "ja" "nah" "fern" "erreichbar" "nein" "301-350" "101-150" "31-50Jahre" "ja" "Wohngebiet" "Zentrum" "1000-1500" "K�che(alt)" "Dusche" "ja" "ja" "ja" "teilm�bliert" "41-50") K))
(print '("EinZimmer" "EG" "Pellets" "nein" "erreichbar" "nah" "erreichbar" "nein" "201-250" "unter50" "51-75Jahre" "ja" "Spielstrasse" "<3km" "2500-3000" "K�che(neu)" "Badewanne" "nein" "nein" "ja" "ja" "51-60"))
(print (is-element-good '("EinZimmer" "EG" "Pellets" "nein" "erreichbar" "nah" "erreichbar" "nein" "201-250" "unter50" "51-75Jahre" "ja" "Spielstrasse" "<3km" "2500-3000" "K�che(neu)" "Badewanne" "nein" "nein" "ja" "ja" "51-60") K))
(print '("2-3Zimmer" "2.Stock" "�l" "nein" "nah" "fern" "nein" "ja" "901-950" "251-300" "1-3Jahre" "ja" "Hauptstrasse" ">30km" "2000-2500" "keine" "DuscheaufFlur" "nein" "ja" "nein" "teilm�bliert" "91-100"))
(print (is-element-good '("2-3Zimmer" "2.Stock" "�l" "nein" "nah" "fern" "nein" "ja" "901-950" "251-300" "1-3Jahre" "ja" "Hauptstrasse" ">30km" "2000-2500" "keine" "DuscheaufFlur" "nein" "ja" "nein" "teilm�bliert" "91-100") K))


(test-all K examples)