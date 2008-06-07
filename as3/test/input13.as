package com.croqodile{

    public class HeartbeatMessage extends ExternalMessage{

		private function handleIncomingMessage(xmlData:XML):void{
			if((xmlData..*::body).length() > 0) {

			}
		}

	}
    
}


