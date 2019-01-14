"use strict";

exports.alert = function (msg) {
  return function (windowObj) {
    return function () {
      if (typeof windowObj.alert !== "function")
        throw new Error(
          "The current window object does not support pop-ups (alert)");
      windowObj.alert(msg);
      return {};
    };
  };
};

exports.confirm = function (msg) {
  return function (windowObj) {
    return function () {
      if (typeof windowObj.confirm !== "function")
        throw new Error("The current window object does not support" +
          " confirmation pop-ups (confirm)");
      return windowObj.confirm(msg);
    };
  };
};

exports._prompt = function (msg) {
  return function (windowObj) {
    return function () {
      if (typeof windowObj.prompt !== "function")
        throw new Error("The current window object does not support" +
          " custom user input (prompt)");
      return windowObj.prompt(msg);
    };
  };
};
