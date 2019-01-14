"use strict";

exports.localStorage = function (windowObj) {
  return function () {
    if (windowObj.localStorage === null ||
      windowObj.localStorage === undefined)
      throw new Error(
        "The current window object does not support local storage");
    return windowObj.localStorage;
  };
};

exports.sessionStorage = function (windowObj) {
  return function () {
    if (windowObj.sessionStorage === null ||
      windowObj.sessionStorage === undefined)
      throw new Error(
        "The current window object does not support session storage");
    return windowObj.sessionStorage;
  };
};
