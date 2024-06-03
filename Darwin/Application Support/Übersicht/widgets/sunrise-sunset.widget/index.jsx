{/*
  * VisualCrossing uses US ZIP codes,
  * or "City,Country" format for location,
  * Country is in ISO-3166-2 ALPHA-2 format
  * (https://www.iban.com/country-codes)
  */}

import { css } from "uebersicht"

const location = 95129
const keyFile  = "$HOME/.VisualCrossingKey"
const key      = "BAD"
const baseURL  = new URL( "https://weather.VisualCrossing.Com/" )
const testURL  = new URL( "http://127.0.0.1:5000/" )
const API      = `/VisualCrossingWebServices/rest/services/timeline/${location}`

const legend = {
    sunrise: "\u263C",
    sunset:  "\u2600"
};

export const refreshFrequency = 1000;

export const initialState = {
    status: 'INIT',
    data: {
        sunrise: 'INIT',
        sunset:  'INIT'
    }
};

{/*
   1. read key from 1password
   2. Use geolocation to not hardcode ${location}
 */}

const weather = new URL(API, testURL)
weather.searchParams.set("key", key);
weather.searchParams.set("contentType", "json");

export const command = (dispatch) => fetch(`${weather.href}`)
    .then((response) => {
        dispatch({ status: 'SUCCESS', data: response.json() })
    })
    .catch((error) => {
        dispatch({ status: 'FAILURE', error: String(error) })
    })

const next = 'AB0012 '

export const updateState = (event, previousState) => {
    switch (event.status) {
    case 'INIT': return event.data;
    case 'SUCCESS': return {
        sunrise: next + String(event.data.currentConditions.sunrise),
        sunset:  event.data.currentConditions.sunset
    };
    case 'FAILURE': return {
        sunrise: "ERRORRET",
        sunset: "<" + event.error + ">"
    };
    default: {
        return {
            sunrise: next + String(event.status),
            sunset:  String(previousState.status)
        };
    }
    }
}

export const render = (p) => {
    return (
        <div className={container}>
            <div className={symbol}>
                <div className={sunRise}>{legend.sunrise}</div>
            </div>
            <div className={timeStamp}>
                <div className={sunRise}>{p.sunrise}</div>
            </div>
            <div className={symbol}>
                <div className={sunRise}>{legend.sunset}</div>
            </div>
            <div className={timeStamp}>
                <div className={sunSet}>{p.sunset}</div>
            </div>
            <div className={debug}>LLL {String(p)}</div>
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
    grid-template-columns: auto auto auto auto;
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

{/*
   Notes:
   - This form of command structure,
     export const command = (dispatch) => {
       fetch(`${weather.href}`)
        .then((response) => {
            return response.json();
        })
        .then((respdata) => {
            return dispatch({ status: 'SUCCESS', data: respdata.curConditions });
        })
        .catch((error) => {
            return dispatch({ status: 'FAILURE', error: String(error) });
        })

        }
     runs repeatedly and throws error, "TypeError: Load failed".
     Why?
   - In this form:
     export const command = (dispatch) => fetch(`${weather.href}`)
      .then((response) => {
        return response.json();
      })
      .then((respdata) => {
        return dispatch({ status: 'SUCCESS', data: respdata.curConditions });
      })
      .catch((error) => {
        return dispatch({ status: 'FAILURE', error: String(error) });
        })
     The event.status is undefined, event itself is an object and I cannot tell its returned fields....


 */}

