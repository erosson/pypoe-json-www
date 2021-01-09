import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const githubUrl = process.env.ELM_APP_GITHUB_URL || "https://github.com/erosson/pypoe-json/tree/master/dist"
const dataUrl = process.env.ELM_APP_DATA_URL || "https://erosson.github.io/pypoe-json/dist"
// const dataUrl = "http://localhost:5000/dist"
const flags = {dataUrl, githubUrl}
const app = Elm.Main.init({flags});

fetch(dataUrl+"/version.json")
.then(res => {
  if (res.status == 200) return res.json()
  else return Promise.reject("bad response status: "+res.status)
})
.then(data => app.ports.fetchedVersion.send({data}))
.catch(error => app.ports.fetchedVersion.send({error: error.message || error.toString()}))

fetch(dataUrl+"/index.json")
.then(res => {
  if (res.status == 200) return res.json()
  else return Promise.reject("bad response status: "+res.status)
})
.then(data => app.ports.fetchedIndex.send({data}))
.catch(error => app.ports.fetchedIndex.send({error: error.message || error.toString()}))

app.ports.fetchDat.subscribe(path => {
  fetch(dataUrl+"/dat/"+path+".json")
  .then(res => {
    if (res.status == 200) return res.json()
    else return Promise.reject("bad response status: "+res.status)
  })
  .then(data => app.ports.fetchedDat.send({data}))
  .catch(error => app.ports.fetchedDat.send({error: error.message || error.toString()}))
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
