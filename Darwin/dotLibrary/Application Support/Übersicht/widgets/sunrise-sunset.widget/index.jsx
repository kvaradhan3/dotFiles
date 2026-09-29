{/*
  * VisualCrossing uses US ZIP codes,
  * or "City,Country" format for location,
  * Country is in ISO-3166-2 ALPHA-2 format
  * (https://www.iban.com/country-codes)
  */}

import { css } from "uebersicht"
import { run } from "uebersicht"

const location  = 95129
const keyFile   = '$HOME/.VisualCrossingKey' // contents: {"key": "MY_KEY"}
const baseURL   = new URL( "https://weather.VisualCrossing.Com/" )
const API       = `VisualCrossingWebServices/rest/services/timeline/`

export const refreshFrequency = 1000 * 60 * 60;
export const initialState = { datetime: "", sunrise: "…", sunset: "…" }

export const command = async (dispatch) => {
    try {
        // 1. Read the API key
        const keyData = await run(`cat "${keyFile}"`)
        const { key } = JSON.parse(keyData)
        if (!key) throw new Error(`no "key" file in "${keyFile}"`)

        // 2. Build the query URL (only request what we need, to lower query cost)
        const url = new URL(API + encodeURIComponent(location), baseURL)
        url.searchParams.set("key", key)
        url.searchParams.set("contentType", "json")
        url.searchParams.set("include", "days,current")
        url.searchParams.set("elements", "datetime,sunrise,sunset")

        // 3. Fetch natively, with a 20-second timeout (like curl --max-time 20)
        const controller = new AbortController()
        const timer = setTimeout(() => controller.abort(), 20000)
        let response
        try {
            response = await fetch(url.href, { signal: controller.signal })
        } finally {
            clearTimeout(timer)
        }

        // Read as text first: VisualCrossing sends plain-text errors
        const body = await response.text()
        if (!response.ok) {
            throw new Error(`HTTP ${response.status}: ${body.slice(0, 200)}`)
        }

        const resp = JSON.parse(body)
        if (!resp.currentConditions) throw new Error("no currentConditions in response")

        dispatch({
            status:   "SUCCESS",
            datetime: `${resp.days[0].datetime}T${resp.currentConditions.datetime}`,
            data:     resp.currentConditions,
        })
    } catch (error) {
        console.error("sunrise widget:", error)
        dispatch({ status: "FAILURE", error: String(error) });
    }
}

export const updateState = (event, previousState) => {
    switch (event.status) {
    case 'SUCCESS':
        return {
            datetime: event.datetime,
            sunrise:  event.data.sunrise,
            sunset:   event.data.sunset,
        };
    case 'FAILURE':
        return {
            datetime: "",
            sunrise:  event.error,
            sunset:   "",
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
    color:                  yellow;
`
const sunSet = css`
    color:                  orange;
`
const debug = css`
    font-family: Helvetica;
    color: blue;
`
