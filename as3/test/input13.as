package com.croqodile{

    public class HeartbeatMessage extends ExternalMessage{

		private function handleIncomingMessage(xmlData:XML):void{
			trace((xmlData..*::body).length())
		}

	}
    
}


