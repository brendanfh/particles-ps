"use strict";

//module Main

exports.requestAnimationFrame = function(f) {
    return function() {
        return window.requestAnimationFrame(function(dt) {
            return f();
        });
    };
};

exports.mapE = function(arr) {
    return function(f) {
        return function() {
            var result = [];
            for(var i=0, l=arr.length; i<l; i++) {
                result.push(f(arr[i])());
            }
            return result;
        }
    }
}