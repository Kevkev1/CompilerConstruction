function Websocket(name, ipaddress, port, onOpen, onMessage, onError, onClose) {
    this.name = name;
    this.ipaddress = ipaddress;
    this.port = port;
    this.url = "ws://"+ipaddress+":"+port+"/";
    this.connected = false;
    this.onOpen = onOpen;
    this.onMessage = onMessage;
    this.onError = onError;
    this.onClose = onClose;

    this.createWebsocket = 
    
    this.connect = function() {
        window.setTimeout(function(parent) {
                                
                                parent.ws = new WebSocket(parent.url);
                                parent.ws.parent = parent;
                                parent.ws.onopen = parent.onOpen;
                                parent.ws.onmessage = parent.onMessage;
                                parent.ws.onerror = parent.onError;
                                parent.ws.onclose = parent.onClose;
                           }, 0, this); //Timeout is needed in order to call the connection code in a seperate thread. The main thread does not stop and wait for the timeout of the websocket.
    }
    
    this.sendMessage = function (message) {
                           console.log("Sending over ws[" + this.name + "]: " + message);
                           this.ws.send(message);
                       };
    
    this.sendJSONObject = function (jsonObject) {
                            this.sendMessage(JSON.stringify(jsonObject));
                          };
    
    this.close = function() {
                    console.log("Closing websocket[" + this.name + "]");
                    this.ws.close();
                 };
}

function doNothing() {}

function setConnected(event) {
    if (this.readyState == 1) {
        this.parent.connected = true;
    }
}

function reportOnOpen(event) {
    console.log(event);
}

function reportOnMessage(message) {
    console.log(message);
}

function reportOnError(event) {
    console.log(event);
}

function retryOnUncleanCloseSetUnconnected(event) {
    this.parent.connected = false;
    retryOnUncleanCloseH(this, event);
}

function retryOnUncleanClose(event) {
    retryOnUncleanCloseH(this, event);
}

function retryOnUncleanCloseH(object, event) {
    console.log("Websocket[" + object.parent.name + "] was closed " + (event.wasClean ? "cleanly" : "not cleanly"));
    if (!event.wasClean || true) {
        object.parent.connect();
    }
}
