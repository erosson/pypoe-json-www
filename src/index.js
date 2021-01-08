import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const flags = {}
const app = Elm.Main.init({flags});

fetch("https://erosson.github.io/pypoe-json/dist/version.json")
.then(res => {
  if (res.status == 200) return res.json()
  else return Promise.reject("bad response status: "+res.status)
})
.then(data => app.ports.fetchedVersion.send({data}))
.catch(error => app.ports.fetchedVersion.send({error: error.message || error.toString()}))

fetch("https://erosson.github.io/pypoe-json/dist/index_dat.json")
.then(res => {
  if (res.status == 200) return res.json()
  else return Promise.reject("bad response status: "+res.status)
})
.then(data => app.ports.fetchedIndexDat.send({data}))
.catch(error => app.ports.fetchedIndexDat.send({error: error.message || error.toString()}))

app.ports.fetchDat.subscribe(path => {
  fetch("https://erosson.github.io/pypoe-json/dist/dat/"+path+".json")
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
