var assets = require('./assets/*.png');
var sounds = require('./assets/*.wav');
var less = require('./src/main.less');
var Main = require('./output/Game');

function main () {
    /*
    Here we could add variables such as

    var baseUrl = process.env.BASE_URL;

    Parcel will replace `process.env.BASE_URL`
    with the string contents of the BASE_URL environment
    variable at bundle/build time.
    A .env file can also be used to override shell variables
    for more information, see https://en.parceljs.org/env.html

    These variables can be supplied to the Main.main function.
    However, you will need to change the type to accept variables, by default it is an Effect.
    You will probably want to make it a function from String -> Effect ()
  */

  window.assets = Object.assign(assets, sounds);
  window.addEventListener('popstate', function(event) {
    window.location.reload();
  });
  Main.main();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    document.body.innerHTML = '';
    main();
  });
}

console.log('Starting app');

main();
