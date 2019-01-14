"use strict";

exports.window = function () {
  if (window === null || window === undefined)
    throw new Error(
      "Window object not found in current environment");
  return window;
};
