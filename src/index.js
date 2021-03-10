import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import queryString from 'query-string'


// TODO:
// * poedat:
//   * split index.json into two files:
//     * `/pypoe/v1/tree/versions.json` (without children)
//     * `/pypoe/v1/tree/$POE_VERSION/index.json`
//   * fix targz directories
//   * poe-version-server version? there were two `3.13.1d`s, one hotfix!
// * www:
//   * version index
//   * version diffs with https://diff.erosson.org/ (json's not in git anymore!)

async function main() {
  const qs = queryString.parse(location.search)
  const dataUrl = process.env.ELM_APP_DATA_URL || qs.dataUrl || "https://poedat.erosson.org"
  // const dataUrl = "http://localhost:5000/dist"
  const flags = {dataUrl}
  const app = Elm.Main.init({flags});

  // Start subscribing to `ports.fetchDat` before `latest.json` finishes fetching.
  // `latest` is a promise; do not immediately `await`!
  let latest = fetchPort(app.ports.fetchedVersion, dataUrl+"/pypoe/v1/latest.json")
  app.ports.fetchDat.subscribe(async ({lang, file}) => {
    const version = (await latest).version
    const path = ["/pypoe/v1/tree", version, lang || "default", file+".min.json"].join("/")
    await fetchPort(app.ports.fetchedDat, dataUrl+path)
  })
  // Subscription's configured, *now* we can `await latest`. It's no longer a promise.
  latest = await latest
  if (latest) {
    await Promise.all([
      fetchPort(app.ports.fetchedLangs, dataUrl+"/pypoe/v1/tree/"+latest.version+"/lang.json"),
      fetchPort(app.ports.fetchedPypoeIndex, dataUrl+"/pypoe/v1/tree/"+latest.version+"/pypoe.json"),
      fetchPort(app.ports.fetchedDatIndex, dataUrl+"/dat/v1/tree/"+latest.version+"/Data/index.json"),
    ])
  }
}
async function fetchPort(port, url) {
  try {
    const res = await fetch(url)
    if (res.status !== 200) {
      throw new Error("bad response status: "+res.status)
    }
    const data = await res.json()
    console.log(url, {data})
    port.send({data})
    return data
  }
  catch (error) {
    port.send({error: url + ": " + (error.message || error.toString())})
    return null
  }
}

main().catch(e => console.error("uncaught: ", e))
// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
