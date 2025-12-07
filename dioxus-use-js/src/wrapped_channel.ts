// 1. Message Type
// 2. Request Id
// 3. Payload? (exists depending on the message Type)
type RequestMessage<T = any> = [number, number] | [number, number, T];

// 1. Request Id
// 2. Success
// 3. Payload? (exists depending on success and the message Type)
type ResponseMessage<T = any> = [number, boolean] | [number, boolean, T];


interface DioxusChannel {
    send: (msg: any) => void;
    recv: () => Promise<any>;
}

const MAX_SAFE_ID: number = Number.MAX_SAFE_INTEGER;

type PendingRequest = {
    resolve: (value?: any) => void;
    reject: () => void;
};

class DioxusRequestResponse {
    private channel: DioxusChannel;
    private currentRequestId: number = 0;

    private pendingRequests: Map<number, PendingRequest> = new Map();

    constructor(channel: DioxusChannel) {
        this.channel = channel;
        this.start();
    }

    private getNextId(): number {
        // 1. Check if the current ID is at the maximum safe limit.
        if (this.currentRequestId >= MAX_SAFE_ID) {
            console.warn(
                `Request ID counter wrapped. Resetting to 0.`
            );
            this.currentRequestId = 0;
        }

        const id = this.currentRequestId;
        this.currentRequestId++;
        return id;
    }

    private async start(): Promise<void> {
        while (true) {
            try {
                // Wait for the next incoming response
                const response: ResponseMessage = await this.channel.recv();
                const requestId = response[0];

                const pending = this.pendingRequests.get(requestId);

                if (!pending) {
                    console.warn(`Unmatched response for ID: ${requestId}`);
                    continue;
                }

                if (response[1]) {
                    if (response.length > 2) {
                        pending.resolve(response[2]);
                    }
                    else {
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

    public async req<ReqPayload = any, ResData = any>(
        type: number,
        payload?: ReqPayload
    ): Promise<ResData> {
        const requestId = this.getNextId();

        let requestMsg: RequestMessage;

        if (payload !== undefined) {
            requestMsg = [type, requestId, payload];
        } else {
            requestMsg = [type, requestId];
        }

        const responsePromise = new Promise<ResData>((resolve, reject) => {
            this.pendingRequests.set(requestId, { resolve, reject });
        });

        this.channel.send(requestMsg);

        return responsePromise;
    }

    public ok(returnValue: any) {
       this.channel.send([true, returnValue]); 
    }

    public err() {
         this.channel.send([false, null]);
    }
}