"use strict";

//module Main

exports.requestAnimationFrame = function(f) {
    return function() {
        window.requestAnimationFrame(function(dt) {
            return f();
        });
    };
};

exports.onMouseMove = function(button) {
    return function(f) {
        return function() {
            document.addEventListener("mousemove", function(e) {
                e.preventDefault();
                if (e.buttons > 0) {
                    if (e.button == button) {
                        f(true)(e.layerX)(e.layerY)();
                    }
                } else {
                    f(false)(e.layerX)(e.layerY)();
                }
                return false;
            }, false);
            document.addEventListener("mousedown", function(e) {
                e.preventDefault();
                if (e.button == button) {
                    f(true)(e.layerX)(e.layerY)();
                }
                return false;
            }, true);
            document.addEventListener("mouseup", function(e) {
                e.preventDefault();
                if (e.button == button) {
                    f(false)(e.layerX)(e.layerY)();
                }
                return false;
            }, true);
            if (button == 2) {
                document.oncontextmenu = function() { return false; };
            }
        }
    }
}

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
