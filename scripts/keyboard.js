var KEYBOARDEVENTS = [ "keypress"
                     , "keyup"
                     ];

var KEYBOARDWS = new Websocket("Keyboard"
                              , IPADDRESS
                              , KEYBOARDPORT
                              , setConnected
                              , doNothing
                              , doNothing
                              , retryOnUncleanCloseSetUnconnected
                              );
        
function keyboardInit() {
    KEYBOARDEVENTS.forEach(function (keyboardEvent, index) {
        document.addEventListener(keyboardEvent, keyboardEventListener);    
    });
    
    KEYBOARDWS.connect();
}


function keyboardEventListener(keyboardEvent) {
    var keyStr = translateKeyboardEventToKeyString(keyboardEvent);
    
    if (keyStr && KEYBOARDWS.connected) {
        KEYBOARDWS.sendJSONObject({"key": keyStr});
    }
}


function translateKeyboardEventToKeyString(keyboardEvent) {
    var keyPressed = false;

    if (keyboardEvent.type == "keypress") {
        switch(keyboardEvent.keyCode) {
            case 32 : keyPressed = "space"; break;
            case 13 : keyPressed = "enter"; break;
            default: keyPressed = String.fromCharCode(keyboardEvent.keyCode); break; 
        }
    } else if (keyboardEvent.type == "keyup") {
       switch(keyboardEvent.keyCode) {
            case 16 : keyPressed = "shift"; break;
            case 17 : keyPressed = "ctrl"; break;
            case 18 : keyPressed = "alt"; break;
            case 8  : keyPressed = "backspace"; break;
            case 20 : keyPressed = "capslock"; break;
            case 38 : keyPressed = "up"; break;
            case 37 : keyPressed = "left"; break;
            case 39 : keyPressed = "right"; break;
            case 40 : keyPressed = "down"; break;
        }
    }
    
    return keyPressed;
}


keyboardInit();