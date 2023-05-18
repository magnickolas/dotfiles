#!/bin/bash
set -e

get_icon() {
	declare -A icons=(
		["01d"]="🌞"
		["02d"]="🌤️"
		["03d"]="⛅️"
		["04d"]="☁️"
		["09d"]="🌦️"
		["10d"]="🌧️"
		["11d"]="⛈️"
		["13d"]="❄️"
		["50d"]="🌫️"
		["01n"]="🌞"
		["02n"]="🌤️"
		["03n"]="⛅️"
		["04n"]="☁️"
		["09n"]="🌦️"
		["10n"]="🌧️"
		["11n"]="⛈️"
		["13n"]="❄️"
		["50n"]="🌫️"
	)
	local icon=${icons[$1]}
	if [[ -n "${icon}" ]]; then
		echo "${icon}"
	else
		echo "…"
	fi
}

LOCATION_LAT=""
LOCATION_LON=""
KEY=""
UNITS="metric"
SYMBOL="°C"

API="https://api.openweathermap.org/data/2.5"

if [ -n "$LOCATION_LAT" ]; then
	location_lat="$LOCATION_LAT"
	location_lon="$LOCATION_LON"
else
	location=$(curl -sf "https://location.services.mozilla.com/v1/geolocate?key=geoclue")
	if [ -n "$location" ]; then
		location_lat="$(echo "$location" | jq '.location.lat')"
		location_lon="$(echo "$location" | jq '.location.lng')"
	else
		exit 2
	fi
fi

current=$(curl -sf "$API/weather?appid=$KEY&lat=$location_lat&lon=$location_lon&units=$UNITS")

if [ -n "$current" ]; then
	current_temp=$(echo "$current" | jq ".main.temp" | cut -d "." -f 1)
	current_icon_code=$(echo "$current" | jq -r ".weather[0].icon")

	echo "$(get_icon "${current_icon_code}") ${current_temp}${SYMBOL}"
else
	exit 1
fi
