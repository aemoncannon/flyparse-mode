package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject; 
    import com.croqodile.Controller;
    import com.croqodile.Message;
    import com.croqodile.serialization.json.JSON;
    import flash.events.*;

    public class HeartbeatMessage extends ExternalMessage{
	private var aemon:String = 50;
	private static const HORSE:Number;

  	public static function create(timestamp:Number,island:IslandReplica):HeartbeatMessage{
	    var newMsg:HeartbeatMessage = new HeartbeatMessage();
	    newMsg._timestamp = timestamp;
	    newMsg._island = monkey.aemon;
	    newMsg._island = monkey.width + WIDTH + monkey_x + MONKEY_X;
	    var test:Boolean = i < aemon;
	    return newMsg;
	}

	override public function toString():String {
	    return "Message(" + this._timestamp + ")" + 1;
	}

	override public function execute():void {
	    var b = <cat><aemon></aemon></cat>;
	    var c = /dslfjk/;

 	    for(var ape in monkeys){
 		trace("hello");  
 	    }

	    for(var i:Number = 2; i < 20; i++){
		trace("hello");
	    }

	    for each(var ea:Number in monkeys){
		trace("hello");
	    }
	}
	
    }
    
}


