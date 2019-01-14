"use strict";

exports._getItem = function (key) {
  return function (storageObj) {
    return function () {
      if (typeof storageObj.getItem !== "function")
        throw new Error("The current storage object does not support" +
          " value retrieval (getItem)");
      return storageObj.getItem(key);
    };
  };
};

exports.setItem = function (key) {
  return function (value) {
    return function (storageObj) {
      return function () {
        if (typeof storageObj.setItem !== "function")
          throw new Error("The current storage object does not support" +
            " value storage (setItem)");
        storageObj.setItem(key, value);
        return {};
      };
    };
  };
};

exports.removeItem = function (key) {
  return function (storageObj) {
    return function () {
      if (typeof storageObj.removeItem !== "function")
        throw new Error("The current storage object does not support" +
          " value removal (removeItem)");
      storageObj.removeItem(key);
      return {};
    };
  };
};
