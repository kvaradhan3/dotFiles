import { css } from "uebersicht";

/** weather codes list can eb fetched from https://weather.codes.
    For the US, you can use the zip code as the default weather code.
    For most countries, you can go to https://weather.codes/{{country}}
    or check at the bottom of https://weather.codes for the list of countries.
 **/
const location = "95129";

/************** UI Settings **************/
const fontColor   = "white";
const itemPadding = "15px";
const fontSize    = "25px";
const fontFamily  = "Helvetica";
const fontSizeTxt = $fontSize;
const fontFamText = $fontFamily;
const fontSizeSym = $fontSize;
const fontFamSymb = "Lucida-Sans Unicode";

export const className = css`
    bottom: 20%;
    left: 20%;
	z-index: 1;
`;

const container = css`
  display: grid;
  grid-template-columns: auto auto auto auto;
`;

const unicode = css`
  font-family: Lucida-Sans Unicode;
  font-size:   "25px";
  color:       ${fontColor};
  border: 0 none;
  padding: 5px;
`;
  
const time = css`
  font-family: "Helvetica";
  font-size:   "25px";
  color:       ${fontColor};
  border: 0 none;
  padding: 5px;
`;
  
/************** UI Settings **************/

export const refreshFrequency = 1000 * 15;

export const command = "\
  curl --silent --no-buffer https://weather.com/today/l/${location} |\
  grep SunriseSunset |\
  grep -E --only-matching '((1[0-2]|0?[1-9]):([0-5][0-9]) ?([AaPp][Mm]))'";

const sunRiseSymbol = "\u263C";
const sunRise = () => {
  return "06:20 am";
}
const sunSetSymbol  = "\u2600";
const sunSet = () => {
  return "04:x3 pm";
}

export const updateState = (event, previousState) => {
  return event;
};

export const render = ({ output }) => {
  return (
    <div className={container}>
      <div>output</div>
      <div className="{unicode}">{sunRiseSymbol}</div>
      <div className="{time}">{sunRise()}</div>
      <div className="{unicode}">{sunSetSymbol}</div>
      <div className="{time}">sunSet()</div>
    </div>
  );
};

