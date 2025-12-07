// wrapped_channel.ts
var MAX_SAFE_ID = Number.MAX_SAFE_INTEGER;

class DioxusRequestResponse {
  channel;
  currentRequestId = 0;
  pendingRequests = new Map;
  constructor(channel) {
    this.channel = channel;
    this.start();
  }
  getNextId() {
    if (this.currentRequestId >= MAX_SAFE_ID) {
      console.warn(`Request ID counter wrapped. Resetting to 0.`);
      this.currentRequestId = 0;
    }
    const id = this.currentRequestId;
    this.currentRequestId++;
    return id;
  }
  async start() {
    while (true) {
      try {
        const response = await this.channel.recv();
        const requestId = response[0];
        const pending = this.pendingRequests.get(requestId);
        if (!pending) {
          console.warn(`Unmatched response for ID: ${requestId}`);
          continue;
        }
        if (response[1]) {
          if (response.length > 2) {
            pending.resolve(response[2]);
          } else {
            pending.resolve();
          }
        } else {
          pending.reject();
        }
        this.pendingRequests.delete(requestId);
      } catch (e) {
        console.error("Critical error receiving message:", e);
        break;
      }
    }
  }
  async req(type, payload) {
    const requestId = this.getNextId();
    let requestMsg;
    if (payload !== undefined) {
      requestMsg = [type, requestId, payload];
    } else {
      requestMsg = [type, requestId];
    }
    const responsePromise = new Promise((resolve, reject) => {
      this.pendingRequests.set(requestId, { resolve, reject });
    });
    this.channel.send(requestMsg);
    return responsePromise;
  }
  ok(returnValue) {
    this.channel.send([true, returnValue]);
  }
  err() {
    this.channel.send([false, null]);
  }
}
