#!/bin/bash

Currencies=(	"EUR" "USD" "GBP" "AED" "ALL" "BTN" "BRL" "BSD" "DZD" "DOP" "JPY" "LSL" "LKR" )
Origins=(	"FR" "UK" "US" "IE" "DE" "PL" "RU" "CN" "SG" "PS" "IM" "IT" "KE" "NU" "TV" )

if [ "$#" -ne 1 ]; then
    echo "Usage: request.sh server"
    exit 1
fi

Server=$1

while true
do
	Trader=$[ 1 + $[ RANDOM % 1000 ] ]	
	FromCurrency=${Currencies[$RANDOM % ${#Currencies[@]} ]}
	ToCurrency=${Currencies[$RANDOM % ${#Currencies[@]} ]}
	Origin=${Origins[$RANDOM % ${#Origins[@]} ]}
	FromAmount=$[ 1 + $[ RANDOM % 10000 ] ]
	ToAmount=$[ 1 + $[ RANDOM % 10000 ] ]
	Rate=$((ToAmount / FromAmount))
	Time=`date +"%d-%b-%y %H:%M:%S" | tr -s '[:lower:]'  '[:upper:]'`
	
	echo "Trader $Trader Converting $FromAmount $FromCurrency to $ToAmount $ToCurrency at rate $Rate from $Origin"

	JSON="{ \"userId\": \"$Trader\", \"currencyFrom\": \"$FromCurrency\", \"currencyTo\": \"$ToCurrency\", \"amountSell\": $FromAmount, \"amountBuy\": $ToAmount, \"rate\": $Rate, \"timePlaced\" : \"$Time\", \"originatingCountry\" : \"$Origin\"}"

	curl 	-H "Accept: */*" \
		-H "Content-Type: application/json" \
		--data "$JSON" $Server

	sleep 0.1
done



