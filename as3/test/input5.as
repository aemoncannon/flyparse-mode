package com.paneplayer {
    import flash.display.*;
    import flash.ui.Keyboard;
    import com.senocular.utils.Output;
    import flash.events.*;
    import flash.net.URLRequest;
    import flash.geom.Point;
    import com.boostworthy.animation.easing.*;
    import com.boostworthy.animation.sequence.*;
    import com.boostworthy.animation.sequence.tweens.*;
    import com.boostworthy.animation.rendering.*;
    import flash.net.NetConnection;
    import flash.net.NetStream;
    import com.adobe.webapis.youtube.*;
    import flash.net.*;

    public class VideoModel {
	public function toXml():Array{
	    return [<videoId>{ this.id }</videoId>,
		    <videoSource>{ this.videoSource }</videoSource>
		    ];
	}

    }
}