{/*
  * VisualCrossing uses US ZIP codes,
  * or "City,Country" format for location,
  * Country is in ISO-3166-2 ALPHA-2 format
  * (https://www.iban.com/country-codes)
  */}

import { css } from "uebersicht"
import { run } from "uebersicht"

const location  = 95129
const keyFile   = '$HOME/.VisualCrossingKey'
const baseURL   = new URL( "https://weather.VisualCrossing.Com/" )
const API       = `/VisualCrossingWebServices/rest/services/timeline/${location}`

export const refreshFrequency = 1000 * 60 * 60;

export const command = (dispatch) => run(`cat "${keyFile}"`)
    .then((output) => {
        let obj = JSON.parse(output);
        return obj.key;
    })
    .then((key) => {
        const weather = new URL(API, baseURL);
        weather.searchParams.set("key", key);
        weather.searchParams.set("contentType", "json");

        return weather.href;
    })
    .then((href) => {
        fetch(href)
            .then((response) => {
                response.json().then((resp) => {
                    var queryDateTime =
                        resp.days[0].datetime + "T" +
                        resp.currentConditions.datetime;
                    dispatch({ status: 'SUCCESS',
                               datetime: queryDateTime,
                               data: resp.currentConditions,
                             });
                });
            });
    })
    .catch((error) => {
        dispatch({ status: 'FAILURE',
                   error: String(error)
                 });
    });

export const updateState = (event, previousState) => {
    switch (event.status) {
    case 'SUCCESS':
        return {
            datetime: event.datetime,
            sunrise: event.data.sunrise,
            sunset:  event.data.sunset,
        };
    case 'FAILURE':
        return {
            datetime: "",
            sunrise: "ERROR",
            sunset: "<" + event.error + ">",
        };
    default:
        console.log("unknown status code: " + String(event.status) +
                    " previous? " + String(previousState.status));
        return previousState;
    }
}

const legend = {
    sunrise: "\u263C",
    sunset:  "\u2600"
};

export const render = (p) => {
    return (
        <div className={container}>
            <div className={timeStamp}>
                <div className={timeStamp_color}>[{p.datetime}] </div>
            </div>
            <div className={symbol}>
                <div className={sunRise}> {legend.sunrise} </div>
            </div>
            <div className={timeStamp}>
                <div className={sunRise}>{p.sunrise} </div>
            </div>
            <div className={symbol}>
                <div className={sunSet}> {legend.sunset} </div>
            </div>
            <div className={timeStamp}>
                <div className={sunSet}>{p.sunset} </div>
            </div>
        </div>
    )
}

export const className = `
  left:    1%;
  bottom:  1%;
  z-index: 1;
  color: grey;
`
const container = css`
    display:               grid;
    grid-template-columns: auto auto auto auto auto;
    border:                0 none;
    text-align:            center;
    vertical-align:        middle;
`
const symbol = css`
    padding:               8px;
    font-family:           Lucida-Sans Unicode;
    font-size:             25px;
`
const timeStamp = css`
    padding:               8px;
    font-family:           Helvetica Neue;
    font-size:             25px;
`
const timeStamp_color = css`
    color:                  0077FF;
`
const sunRise = css`
    color:                  blue;
`
const sunSet = css`
    color:                  orange;
`
const debug = css`
    font-family: Helvetica;
    color: blue;
`
