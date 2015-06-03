var MOUSEEVENTS = [ "click"
                  , "dblclick"
                  , "mousedown"
                  , "mouseup"
                  , "mouseenter"
                  , "mouseleave"
                  , "mousemove"
                  ];
                  
var MOUSEWS = new Websocket( "Mouse"
                           , IPADDRESS
                           , MOUSEPORT
                           , setConnected
                           , doNothing
                           , doNothing
                           , retryOnUncleanCloseSetUnconnected
                           );

function mouseInit() {
    MOUSEEVENTS.forEach(function (mouseEvent, index) {
        document.addEventListener(mouseEvent, mouseEventListener);    
    });
    
    MOUSEWS.connect();
}


function mouseEventListener(mouseEvent) {
    var jsonObject = { "type": mouseEvent.type
                     , "button": toMouseButton(mouseEvent.button)
                     , "x": mouseEvent.clientX
                     , "y": mouseEvent.clientY
                     };
    
    if (MOUSEWS.connected) {
        MOUSEWS.sendJSONObject(jsonObject);
    }
}

function toMouseButton(which) {
    var button = "";
    
    switch (which) {
        case 0 : button = "left"; break;
        case 1 : button = "middle"; break;
        case 2 : button = "right"; break;
    }
    
    return button;
}

mouseInit();