"use strict";

var observers = [];

function innerHandler(event) {
  if (observers.length != 0) {
    observers.forEach(function(observer) {
      observer(event);
    });
  }
}

// Start listening for keys
// :: Effect Unit
exports.startListening = function() {
  document.addEventListener('keydown', innerHandler);
};

// Stop listening for keys
// :: Effect Unit
exports.stopListening = function() {
  document.removeEventListener('keydown', innerHandler);
};

// Await a key
// :: EffectFnAff KeyEvent
exports._awaitKey = function (onError, onSuccess) {
  var index = observers.length;
  observers.push(onSuccess);
  return function (cancelError, onCancelerError, onCancelerSuccess) {
    onCancelerSuccess();
    observers = observers.slice(index);
  };
};
