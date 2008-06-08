package com.paneplayer.remote {
	import com.adobe.crypto.*;
	import com.adobe.serialization.json.*;
	import com.paneplayer.events.*;
	import com.paneplayer.utils.*;
	import com.paneplayer.*;
	import com.senocular.utils.Output;
	import com.paneplayer.model.*;
	import com.paneplayer.dialogs.*;
	import flash.events.*;
	import flash.net.*;
	import flash.system.LoaderContext;
	import flash.utils.Timer;
	import flash.xml.*;
	
	/**
	* Manage communications with the uvLayer server component.
	*/	
	public class ServiceController extends EventDispatcher{
		private var _host:String;
		private var _greenpointBase:String;
		private var _user:User;
		private var _sharedVidsTimer:Timer;
		private var _autoSaveTimer:Timer;
		private var _recentlyViewedTimer:Timer;
		private var _updateActiveStatusTimer:Timer;
		private var _updateStatsTimer	:Timer;

		// Updated whenever we poll for video view events,
		// so that we can hand back to server and only
		// get fresh events next time.
		private var _lastVideoViewCheckTimestamp:Number;

		private static const ONE_DAY_IN_MSECS:Number = 86400000;
		private static const ONE_HOUR_IN_MSECS:Number = 3600000;
		private static const ONE_WEEK_IN_MSECS:Number = ONE_DAY_IN_MSECS * 7;
		public static const USER_TYPE_ANON:String = "ANONYMOUS";
		public static const USER_TYPE_STANDARD:String = "STANDARD";
		
		public static const SERVICE_FACEBOOK:String = "FACEBOOK";
		public static const SERVICE_YOUTUBE:String = "YOUTUBE";
		public static const SERVICE_TRUVEO:String = "TRUVEO";
		public static const SERVICE_INEXT:String = "INDUSTRYNEXT";

		public static const SOCIAL_TYPE_FACEBOOK:String = "FACEBOOK";
		public static const SOCIAL_TYPE_JABBER:String = "JABBER";
		public static const SOCIAL_TYPE_EMAIL:String = "EMAIL";
		
		private static const NEW_VIDS_POLL_TIME:Number = 10000;
		private static const AUTO_SAVE_TIME:Number = 30000;
		private static const CHECK_EVENTS_TIME:Number = 20000;
		private static const USER_STATUS_TIME:Number = 17000;
		private static const UPDATE_STATS_TIME:int = 13000;

		private static const DEFAULT_HOST:String = "http://uvlayer.com";
		//public static const DEFAULT_HOST:String = "http://216.57.20.59:8180/";

		//Paths are relative to DEFAULT_HOST
		private static const DEFAULT_GREENPOINT_BASE:String = "Greenpoint";
		private static const JSON_ENDPOINT_PATH:String = "/HttpDispatch";
		private static const DEMO_SWF_PATH:String = "/Bushwick/cropped.swf";
		public static const UPDATE_FILE_NAME:String = "uvLayer-client-%s.air";
		private static const UPDATE_FILE_PATH:String = "/Bushwick/uvLayer-client-%s.air";
		private static const VERSION_FILE_PATH:String = "/Bushwick/uvLayer-client-version.json";

		private static const BULLETIN_METADATA_PATH  :String = "/Bushwick/bulletin/bulletins.json";
		private static const BULLETIN_PATH                :String = "/Bushwick/bulletin/%s";

		public static const YOUTUBE_VIDEO_URL:String = "http://www.youtube.com/get_video?video_id=%s&t=%s";
		public static const YOUTUBE_SWF_URL:String = "http://www.youtube.com/v/%s";
		public static const YOUTUBE_VIDEO_URL_REGEX:RegExp = /http:\/\/(?:www\.)?youtube\.com\/watch\?v=([^\.\/&]+)/;

		public static const UVLAYER_COM_SHARING_URL:String = "http://www.uvlayer.com?videoIds=%s&uvUser=%s&uvAvatar=%s";

		public static const UVLAYER_EMBED_HTML:String = "<div id='embedContainer' style='border: 1px solid #BBBBBB;width:500px;'></div><script type='text/javascript' src='http://www.uvlayer.com/js/swfobject1-5/swfobject.js'></script><script type='text/javascript' src='http://www.uvlayer.com/js/jquery-1.2.2.min.js'></script><script type='text/javascript' src='http://www.uvlayer.com/js/browser_detect.js'></script><script type='text/javascript'>  var videoIds = [%s];    var pageUrl = 'http://uvlayer.com?videoIds=' + videoIds.join(',');   var so = new SWFObject('http://www.uvlayer.com/uvlayer_lite.swf', 'uvlayer', '500', '400', '9.0.45', '#FFFFFF');     so.addVariable( 'pageUrl', escape(pageUrl) );         so.write('embedContainer');  </script>";

		public static const UVLAYER_COM_FEEDBACK_URL:String = "http://www.uvlayer.com/feedback.jsp";
		public static const EULA_URL :String = "http://www.uvlayer.com/docs/uvLayer_Terms_of_Service.html";
		public static const HELP_URL:String = "http://uvlayer.com/docs/uvlayerhelp/";
		public static const DOWNLOAD_UVLAYER_URL:String = "http://uvlayer.com/download.html";

		private static const GREENPOINT_CROSS_DOMAIN_XML_PATH:String = "/crossdomain.xml";

		private static const IMAGE_PROXY_CROSS_DOMAIN_PATH:String = "/crossdomain.xml";
		private static const IMAGE_PROXY_PATH:String = "/ImageProxy?url=%s";

		private static const SERVICE_RESULT_SUCCESS:String = "SUCCESS";

		// Note that this is here, and not in FacebookServiceController, intentionally,
		// because it's a service provided by Greenpoint.
		private static const FACEBOOK_PROFILE_FBML_URL:String = "/facebook/profile_fbml.jsp?infbid=%s&inbkid=%s";
		
		// JSON Dispatch Method codes

		public static const METH_RESET_PASSWORD:String = "RESET_PASSWORD";
		public static const METH_ADD_USER_SOCIAL_ID:String = "ADD_USER_SOCIAL_ID";
		public static const METH_GET_USER_SOCIAL_IDS:String = "GET_USER_SOCIAL_IDS";
		public static const METH_SOCIAL_ID_TO_USER_ID:String = "SOCIAL_ID_TO_USER_ID";
		private static const METH_SAVE_CANVAS:String = "SAVE_CANVAS";
		private static const METH_ADD_COMMENT:String = "ADD_ITEM_COMMENT";
		private static const METH_FILTER_USERS:String = "FRIEND_ID_TO_USER_ID";
		private static const METH_CREATE_USER:String = "CREATE_USER";
		private static const METH_UPDATE_USER:String = "UPDATE_USER";
		private static const METH_LOGIN_USER:String = "LOGIN_USER";
		private static const METH_LOAD_CANVAS:String = "GET_CANVAS_BY_USER";
		private static const METH_ITEM_PROPERTIES:String = "GET_ITEM_PROPERTIES";
		private static const METH_FLV_URL:String = "GET_FLV_URL";
		private static const METH_TITLE_SEARCH:String = "GET_VIDEOS_BY_TITLE";
		private static const METH_TAG_SEARCH:String = "GET_VIDEOS_BY_TAGS";
		private static const METH_NEW_VIDEOS:String = "GET_NEW_VIDEOS";
		private static const METH_POPULAR_VIDEOS:String = "GET_POPULAR_VIDEOS";
		private static const METH_USER_SEARCH:String = "GET_VIDEOS_BY_USER";
		private static const METH_SHARE_ITEMS:String = "SHARE_ITEMS";
		private static const METH_GET_SHARED_ITEMS:String = "GET_SHARED_ITEMS";
		private static const METH_TRACK_VIEW_VIDEO:String = "VIEW_VIDEO";
		private static const METH_GET_ITEM_COMMENTS:String = "GET_ITEM_COMMENTS";
		private static const METH_GET_VIEWED_VIDEOS = "GET_VIEWED_VIDEOS";
		private static const METH_GET_VIDEO_VIEWERS = "GET_VIDEO_VIEWERS";
		private static const METH_GET_USER_METRICS = "GET_USER_METRICS";
		private static const METH_GET_USER_ACTIVE_STATUS = "GET_USER_ACTIVE_STATUS";

		public function get host():String{ return _host }

		public function get greenpointBase():String{ return _greenpointBase }

		public function get jsonEndpoint():String{
			return host + "/" + greenpointBase + "/" + JSON_ENDPOINT_PATH;
		}

		public function bulletinUrl(name:String):String{
			return host + Format.str(BULLETIN_PATH, [name]);
		}

		public function get bulletinMetadataUrl():String{
			return host + BULLETIN_METADATA_PATH;
		}

		public function get greenpointCrossdomainUrl():String{
			return host + "/" + greenpointBase + "/" + GREENPOINT_CROSS_DOMAIN_XML_PATH;
		}

		public function get imageProxyCrossdomainUrl():String{
			return host + "/" + greenpointBase + "/" + IMAGE_PROXY_CROSS_DOMAIN_PATH;
		}

		public function get jabberServerCrossdomainUrl():String{
			return "xmlsocket://uvlayer.com:5229";
		}

		public function get youtubeImageCrossdomainUrl():String{
			return "http://img.youtube.com/crossdomain.xml";
		}

		public function get imageProxyUrl():String{
			return host + "/" + greenpointBase + "/" + IMAGE_PROXY_PATH;
		}
		
		public function get demoSwfUrl():String{
			return host + DEMO_SWF_PATH;
		}

		public function updateFileLocation(version:String):String{
			return host + Format.str(UPDATE_FILE_PATH, [version]);
		}

		public function get versionFileLocation():String{
			return host + VERSION_FILE_PATH;
		}

		private function get user():User{ return _user; }
		
		public function ServiceController(user:User, hostUrl:String = DEFAULT_HOST, greenpointBase:String = DEFAULT_GREENPOINT_BASE){
			_user = user;
			_host = hostUrl || DEFAULT_HOST;
			_greenpointBase = greenpointBase || DEFAULT_GREENPOINT_BASE;
			_lastVideoViewCheckTimestamp = (new Date()).getTime() - (10 * ONE_HOUR_IN_MSECS);
		}
		
		public function startPeriodicEvents(){
			stopPeriodicEvents();
			_sharedVidsTimer = new Timer(NEW_VIDS_POLL_TIME);
			_sharedVidsTimer.addEventListener(TimerEvent.TIMER, onSharedVidsTimer);
			_sharedVidsTimer.start();
			
			_autoSaveTimer = new Timer(AUTO_SAVE_TIME);
			_autoSaveTimer.addEventListener(TimerEvent.TIMER, onAutoSaveTimer);
			_autoSaveTimer.start();
			
			_recentlyViewedTimer = new Timer(CHECK_EVENTS_TIME);
			_recentlyViewedTimer.addEventListener(TimerEvent.TIMER, onRecentlyViewedTimer);
			_recentlyViewedTimer.start();

			_updateActiveStatusTimer = new Timer(USER_STATUS_TIME);
			_updateActiveStatusTimer.addEventListener(TimerEvent.TIMER, onUpdateActiveStatus);
			_updateActiveStatusTimer.start();

			_updateStatsTimer = new Timer(UPDATE_STATS_TIME);
			_updateStatsTimer.addEventListener(TimerEvent.TIMER, onUpdateStatistics );
			_updateStatsTimer.start();

			// Force-fire a couple events
 			onSharedVidsTimer(null);
 			onRecentlyViewedTimer(null);
 			onUpdateActiveStatus(null);
 			onUpdateStatistics(null);
		}
		
		public function stopPeriodicEvents(){
			if(_sharedVidsTimer){
				_sharedVidsTimer.removeEventListener(TimerEvent.TIMER, onSharedVidsTimer);
				_sharedVidsTimer.stop();
			}
			
			if(_autoSaveTimer){
				_autoSaveTimer.removeEventListener(TimerEvent.TIMER, onSharedVidsTimer);
				_autoSaveTimer.stop();
			}
			
			if(_recentlyViewedTimer){
				_recentlyViewedTimer.removeEventListener(TimerEvent.TIMER, onSharedVidsTimer);
				_recentlyViewedTimer.stop();
			}

			if(_updateActiveStatusTimer){
				_updateActiveStatusTimer.removeEventListener(TimerEvent.TIMER, onUpdateActiveStatus);
				_updateActiveStatusTimer.stop();
			}

			if(_updateStatsTimer){
				_updateStatsTimer.removeEventListener(TimerEvent.TIMER, onUpdateStatistics);
				_updateStatsTimer.stop();
			}
		}
		

		/** PERIODIC EVENTS **/

		/**
		* Periodically check for videos shared with the current user.
		*
		* @param e 
		* @return 
		*/		
		private function onSharedVidsTimer(e:TimerEvent):void{
			if(App.ref.external.userIsActive){
				getSharedVids(function(vids, groups){
						if(vids.length > 0 || groups.length > 0){
							dispatchEvent(new NewSharedVideosEvent(vids, groups));
						}
					});
			}
		}


		/**
		* Periodically check for new, recently viewed videos events.
		* @param e 
		* @return 
		*/		
		private function onRecentlyViewedTimer(e:TimerEvent):void{
			if(App.ref.external.userIsActive){
				getRecentlyViewedVideos(App.ref.friendsController.roster.uvContacts, 
					function(pairs:Array){
						if(pairs.length > 0){
							var i:Number = Math.max(0, pairs.length - 3);
							while(i < pairs.length){
								var vid:VideoModel = pairs[i][0];
								var contact:ContactModel = pairs[i][1];
								dispatchEvent(new NewVideoViewedEvent(vid, contact));
								i++;
							}
						}
					});
			}
		}


		/**
		* Periodically check for changes in the active status of our friends
		* @param e 
		* @return 
		*/		
		private function onUpdateActiveStatus(e:TimerEvent):void{
			if(App.ref.external.userIsActive){
				getUserActiveStatus(App.ref.friendsController.roster.uvContacts, 
					function(activityHash:Object){
						dispatchEvent(new UserActivityStatusUpdatedEvent(activityHash));
					});
			}
		}

		/**
		* Periodically check for changes in the rankings of your friends
		* @param e 
		* @return 
		*/		
		private function onUpdateStatistics(e:TimerEvent):void{
			if(App.ref.external.userIsActive){
				getUserMetrics(function(statsHash:Object){
						dispatchEvent(new UserStatisticsUpdatedEvent(statsHash));
					});
			}
		}


		/**
		* Periodically save the user's canvas
		* 
		* @param e 
		* @return 
		*/		
		private function onAutoSaveTimer(e:TimerEvent):void{
			if(App.ref.external.userIsActive){
				saveCanvas(function(ignore){}, function(){});
			}
		}

		/** END PERIODIC EVENTS **/




		/**
		* Given an array of facebook contacts, friends, filter out all those who do not
		* have a corresponding account with uvLayer. Callback with the remaining, filtered
		* contacts.
		*
		* @param friends 
		* @param facebookUid 
		* @param callback 
		* @return A new, filtered array.
		*/		
		public function filterFriends(friends:EnumArray, facebookUid:String, callback:Function):void{
			var params:Array = [["userId", user.userId],
				["user_foreign_id", facebookUid]];
			for each(var contact:ContactModel in friends){
				params.push(["friend_id", contact.sourceId]);
			}
			dispatchServiceParamsPost(METH_FILTER_USERS, params, function(e:Event){
					parseFilterFriendsResponse(URLLoader(e.target).data, friends, callback);
				});
		}
		private function parseFilterFriendsResponse(json:String, friends:EnumArray, callback:Function):void{
			withServiceResultValue(json, function(results:Array){
					if(results.length > 0){
						var finalFriends:EnumArray = new EnumArray();
						for each(var user:Object in results){
							var inter:EnumArray = friends.select(function(ea:ContactModel){
									return (ea.sourceId == user.facebookId);
								});
							if(inter.notEmpty){
								var first:ContactModel = inter.first;
								first.userId = String(user.id);
								finalFriends.push(first);
							}
						}

						callback(finalFriends);
					}
					else{
						Dialog.open(new FailureDialog(Locale.str(Locale.FACEBOOK_FRIENDS_LOAD_EMPTY)));
					}
				},
				function(){
					Dialog.open(new FailureDialog(Locale.str(Locale.FACEBOOK_FRIENDS_LOAD_ERROR)));
				},
				isArrayP
			);
		}
		
		/**
		* Issue a tracking event to server recording the watching of vid. 
		*
		* @param vid 
		* @param resultsCallback This function is called with result of the server call.
		* @return 
		*/		
		public function trackViewVideo(vid:VideoModel, callback:Function):void{
			var params:Array = [["userId", user.userId],
				["itemId", vid.serviceId],
				["serviceName", vid.serviceProvider]];

			dispatchServiceParamsPost(METH_TRACK_VIEW_VIDEO, params, function(e:Event){
					withServiceResultValue(URLLoader(e.target).data, callback, ignoreFailure);
				});
		}


		/**
		* Make a new association between user's uvlayer id and some social networking id.
		* 
		* @param socialType 
		* @param socialId 
		* @param callback 
		* @return 
		*/		
		public function addUserSocialId(socialType:String, socialId:String, callback:Function = null):void{
			var params:Array = [["userId", user.userId],
				["socialIdType", socialType],
				["user_social_id", socialId]];
			dispatchServiceParamsPost(METH_ADD_USER_SOCIAL_ID, params, function(e:Event){
					withServiceResultValue(URLLoader(e.target).data, callback || function(val){}, ignoreFailure, isNullP);
				});
		}


		/**
		* Given a userId, get all the social network ids that are associated with this id.
		* 
		* @param user 
		* @param callback 
		* @param failCallback 
		* @return 
		*/		
		public function getUserSocialIds(callback:Function, failCallback:Function):void{
			var params:Array = [["userId", user.userId]];
			dispatchServiceParamsPost(METH_GET_USER_SOCIAL_IDS, params, function(e:Event){
					withServiceResultValue(URLLoader(e.target).data, function(results:Array){
							callback(new EnumArray(results));
						},
						failCallback,
						isArrayP
					);
				});
		}


		/**
		* Given a list of friend's social network ids, lookup their uvlayer ids.
		* 
		* @param user 
		* @param socialType 
		* @param friendIds 
		* @param callback 
		* @param failCallback 
		* @return 
		*/		
		public function mapSocialIdToUserId(socialType:String, friendIds:EnumArray, 
			callback:Function, failCallback:Function):void{
			var params:Array = [
				["userId", user.userId],
				["socialIdType", socialType]
			];
			for each(var id:String in friendIds){
				params.push(["friend_id", id]);
			}
			dispatchServiceParamsPost(METH_SOCIAL_ID_TO_USER_ID, params, function(e:Event){
					withServiceResultValue(URLLoader(e.target).data, function(results:Array){
							callback(new EnumArray(results));
						},
						failCallback,
						isArrayP
					);
				});
		}

		/**
		* Check for any new videos shared with current user. If any exist,
		* pass them to resultsCallback
		*
		* @param resultsCallback 
		* @return 
		*/		
		public function getSharedVids(resultsCallback:Function):void{
			var params:Array = [["userId", user.userId]];
			dispatchServiceParamsPost(METH_GET_SHARED_ITEMS, params, function(e:Event){
					parseGetSharedVidsResponse(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseGetSharedVidsResponse(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Array){
					var vids:EnumArray = new EnumArray();
					var collections:EnumArray = new EnumArray();
					for each(var group in results){
						if(group.id == 0 && group.items.length > 0){ // These are videos shared one at a time..
							vids.pushAll(VideoModel.manyFromJson(group.items));
						}
						else if(group.items.length > 0){ // This is a group, shared as a group..
							group.type = CanvasModel.GROUP_TYPE_IN_INBOX;
							collections.push(CollectionModel.fromJson(group));
						}
					}
					resultsCallback(vids, collections);
				},
				ignoreFailure,
				isArrayP
			);
		}
		
		/**
		* Get comments of vid that are newer than timestamp. Pass them to resultsCallback.
		* 
		* @param vid 
		* @param timestamp A time in seconds since epoch.
		* @param resultsCallback 
		* @return 
		*/		
		public function getVideoComments(vid:VideoModel, contacts:EnumArray, timestamp:Number, resultsCallback:Function):void{
			var params:Array = [["userId", user.userId],
				["serviceName", vid.serviceProvider],
				["timestamp", timestamp],
				["itemId", vid.serviceId]];
			contacts.each(function(contact:ContactModel){
					params.push(["friend_id", contact.userId]);
				});
			dispatchServiceParamsPost(METH_GET_ITEM_COMMENTS, params, function(e:Event){
					parseGetVideoComments(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseGetVideoComments(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Array){
					var comments:Array = [];
					for each(var item in results){ 
						comments.push(new CommentModel(item.body, 
								item.author.id,
								Number(item.timeStamp)));
					}
					resultsCallback(comments);
				},
				ignoreFailure,
				isArrayP
			);
		}


		/**
		* Callback with a list of videos recently viewed by current user's friends.
		* Returned users are no older than _lastVideoViewCheckTimestamp
		* 
		* @param resultsCallback 
		* @return 
		*/		
		public function getRecentlyViewedVideos(contacts:EnumArray, resultsCallback:Function):void{
			var params:Array = [
				["userId", user.userId],
				["timestamp", _lastVideoViewCheckTimestamp]
			];
			contacts.each(function(contact:ContactModel){
					params.push(["friend_id", contact.userId]);
				});
			dispatchServiceParamsPost(METH_GET_VIEWED_VIDEOS, params, function(e:Event){
					parseGetRecentlyViewedVideos(URLLoader(e.target).data, 
						resultsCallback);
				});
		}
		private function parseGetRecentlyViewedVideos(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Array){
					var pairs:EnumArray = new EnumArray();
					for each(var item in results){ 
						var userId:String = item.viewedByUser.id;
						var contact:ContactModel = ContactModel.forUserId(userId);
						var vid:VideoModel = VideoModel.fromJson(item);
						if(contact && (contact.userId != user.userId)){
							pairs.push([vid, contact]);
						}
					}
					if(results.length > 0){
						_lastVideoViewCheckTimestamp = Number(results[0].viewTimestamp);
					}
					resultsCallback(pairs.reversed());
				},
				ignoreFailure,
				isArrayP
			);
		}


		/**
		* Callback with a list of videos recently viewed by 'contact'
		* 
		* @param contact 
		* @param resultsCallback 
		*/		
		public function getRecentlyViewedVideosFor(contact:ContactModel, resultsCallback:Function):void{
			var params:Array = [
				["userId", user.userId],
				["limit", 8],
				["friend_id", contact.userId]
			];
			dispatchServiceParamsPost(METH_GET_VIEWED_VIDEOS, params, function(e:Event){
					parseGetRecentlyViewedVideosFor(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseGetRecentlyViewedVideosFor(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Array){
					var result:EnumArray = (new EnumArray(results).collect(VideoModel.fromJson)).reversed();
					resultsCallback(result);
				},
				ignoreFailure,
				isArrayP
			);
		}


		/**
		* Callback with a list of my friends who recently viewed 'video'
		* 
		* @param video 
		* @param resultsCallback 
		*/		
		public function getRecentViewersOf(video:VideoModel, contacts:EnumArray, resultsCallback:Function):void{
			var params:Array = [
				["userId", user.userId],
				["limit", 10],
				["itemId", video.serviceId],
				["serviceName", video.serviceProvider]
			];
			contacts.each(function(contact:ContactModel){
					params.push(["friend_id", contact.userId]);
				});
			dispatchServiceParamsPost(METH_GET_VIDEO_VIEWERS, params, function(e:Event){
					parseGetRecentViewersOf(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseGetRecentViewersOf(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Array){
					resultsCallback(new EnumArray(results).collect(function(ea){ return ea.id}));
				},
				ignoreFailure,
				isArrayP
			);
		}

		/**
		* Callback with a hash of userId:String -> status:Boolean pairs
		* 
		* @param video 
		* @param resultsCallback 
		*/
		public function getUserActiveStatus(contacts:EnumArray, resultsCallback:Function):void{
			var params:Array = [
				["userId", user.userId],
				["timestamp", (new Date()).getTime() - ONE_DAY_IN_MSECS]
			];
			contacts.each(function(contact:ContactModel){
					params.push(["friend_id", contact.userId]);
				});
			dispatchServiceParamsPost(METH_GET_USER_ACTIVE_STATUS, params, function(e:Event){
					parseGetUserActiveStatus(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseGetUserActiveStatus(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Object){
					resultsCallback(results);
				},
				ignoreFailure,
				isObjP
			);
		}


		/**
		* Request a summary of the current user's statistics. Pass them to resultsCallback.
		* Stats include a count of total video views and the user's overall approval rating.
		* 
		* @param resultsCallback 
		* @return 
		*/		
		public function getUserMetrics(resultsCallback:Function):void{
			var params:Array = [["userId", user.userId],
				["timestamp", (new Date()).getTime() - ONE_DAY_IN_MSECS]];
			dispatchServiceParamsPost(METH_GET_USER_METRICS, params, function(e:Event){
					parseGetUserMetrics(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseGetUserMetrics(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(result:Object){
					resultsCallback(result);
				},
				ignoreFailure,
				isObjP
			);
			
		}
		
		
		/**
		* Send vid to contact, attaching msg as a comment in the process.
		* 
		* @param vid 
		* @param msg 
		* @param contact 
		* @return 
		*/		
		public function shareVideo(vid:VideoModel, msg:String, contact:ContactModel):void{
			var params:Array = [
				["userId", user.userId],
				["friend_id", contact.userId],
				["itemId", vid.serviceId],
				["serviceName", vid.serviceProvider]
			];
			if(msg.length > 0){
				params.push(["comment", msg]);
			}
			dispatchServiceParamsPost(METH_SHARE_ITEMS, params, function(e:Event){
					parseShareVideoResponse(URLLoader(e.target).data);
				});
		}
		private function parseShareVideoResponse(json:String):void{
			withServiceResultValue(json, function(results:Object){
				},
				ignoreFailure,
				isObjP
			);
		}
		

		/**
		* Send collection to contact, attaching msg as a comment in the process.
		* 
		* @param group 
		* @param msg 
		* @param contact 
		* @return 
		*/		
		public function shareCollection(collection:CollectionModel, msg:String, contact:ContactModel):void{
			var params:Array = [
				["groupName", collection.name],
				["userId", user.userId],
				["friend_id", contact.userId]
			];
			collection.items.each(function(ea:ContentModel){
					params.push(["itemId", ea.serviceId]),
					params.push(["serviceName", ea.serviceProvider])
				});
			dispatchServiceParamsPost(METH_SHARE_ITEMS, params, function(e:Event){
					parseShareGroupResponse(URLLoader(e.target).data);
				});
		}
		private function parseShareGroupResponse(json:String):void{
			withServiceResultValue(json, function(results:Object){
				},
				ignoreFailure,
				isObjP
			);
		}
		


		/**
		* Add a comment with text of msg to vid. Pass response to callback.
		* 
		* @param vid 
		* @param msg 
		* @param callback 
		* @return 
		*/		
		public function addComment(vid:VideoModel, msg:String, callback:Function):void{
			var params:Array = [
				["userId", user.userId],
				["itemId", vid.serviceId],
				["serviceName", vid.serviceProvider],
				["comment", msg]
			];
			dispatchServiceParamsPost(METH_ADD_COMMENT, params, function(e:Event){
					parseAddCommentResponse(URLLoader(e.target).data, callback);
				});
		}
		private function parseAddCommentResponse(json:String, callback:Function):void{
			withServiceResultValue(json, function(result){
					callback();
				},
				ignoreFailure,
				isNullP
			);
		}
		
		/**
		* Create a new user. On success, invoke callback with the newly created user's userId.
		* 
		* @param username 
		* @param password The plain text password for this user
		* @param email An email, currently unused
		* @param callback A function to be invoked on success.
		* @param failCallback A function to be invoked on failure.
		* @return 
		*/		
		public function createUser(callback:Function, failCallback:Function, username:String = null, password:String = null){
			var params:Array = [];
			if(username){ params.push(["userName", username])}
			if(password){ params.push(["passwordHash", MD5.hash(password)])}
			dispatchServiceParamsPost(METH_CREATE_USER, params, function(e:Event){
					parseCreateUserResponse(URLLoader(e.target).data, callback, failCallback);
				});
		}
		private function parseCreateUserResponse(json:String, callback:Function, failCallback:Function):void{
			withServiceResultValue(json, function(userData:Object){
					callback(userData.id)
				}, 
				failCallback,
				function(userData):Boolean{ return isObjP(userData) && userData.id is String }
			);
		}


		/**
		* Update the user information for the currently active user.
		* 
		* @param callback 
		* @param failCallback 
		* @param username 
		* @param password 
		* @return 
		*/		
		public function updateUser(callback:Function, failCallback:Function, username:String = null, password:String = null){
			var params:Array = [];
			params.push(["userId", user.userId])
			if(username){ params.push(["userName", username])}
			if(password){ params.push(["passwordHash", MD5.hash(password)])}
			dispatchServiceParamsPost(METH_UPDATE_USER, params, function(e:Event){
					parseUpdateUserResponse(URLLoader(e.target).data, callback, failCallback);
				});
		}
		private function parseUpdateUserResponse(json:String, callback:Function, failCallback:Function):void{
			withServiceResultValue(json, function(result){
					callback();
				}, 
				failCallback,
				isNullP
			);
		}
		
		/**
		* Essentially, get the user id for a username/password combination.
		* 
		* @param username 
		* @param password A plain text password.
		* @param successCallback Function to be invoked on success.
		* @param failCallback Function to be invoked on failure.
		* @return 
		*/		
		public function loginUser(username:String, password:String, successCallback:Function, failCallback:Function){
			var params:Array = [["userName", username],
				["passwordHash", MD5.hash(password)]];
			dispatchServiceParamsPost(METH_LOGIN_USER, params, function(e:Event){
					parseLoginUserResponse(URLLoader(e.target).data, successCallback, failCallback);
				});
		}
		private function parseLoginUserResponse(json:String, successCallback:Function, failCallback:Function):void{
			withServiceResultValue(json, function(userData:Object){
					successCallback(userData.id);
				},
				failCallback,
				function(userData):Boolean{ return isObjP(userData) && userData.id is String }
			);
		}

		/**
		* Given a username, which must be a valid username and email in the uvLayer system, 
		* reset the password corresponding to the username to some random password.
		* This password will then be emailed to the given address (username).
		* 
		* @param username (must be a valid email)
		* @param successCallback 
		* @param failCallback 
		* @return 
		*/		
		public function resetPassword(username:String, successCallback:Function, failCallback:Function){
			var params:Array = [["userName", username]];
			dispatchServiceParamsPost(METH_RESET_PASSWORD, params, function(e:Event){
					parseResetPasswordResponse(URLLoader(e.target).data, successCallback, failCallback);
				});
		}
		private function parseResetPasswordResponse(json:String, successCallback:Function, failCallback:Function):void{
			withServiceResultValue(json, function(userData:Object){
					successCallback();
				},
				failCallback,
				isObjP
			);
		}

		/**
		* Get the details for a video id. Useful when all we have is an id.
		* 
		* @param videoId 
		* @param service A service provider, YOUTUBE, TRUVEO etc...
		* @param callback 
		* @return 
		*/		
		public function lookupVideo(videoId:String, service:String, callback:Function):void{ 
			var params:Array = [["sourceId", videoId],
				["serviceName", service]];
			dispatchServiceParamsPost(METH_ITEM_PROPERTIES, params, function(e:Event){
					parseLookupVideoResponse(URLLoader(e.target).data, callback);
				});
		}
		private function parseLookupVideoResponse(json:String, callback:Function):void{
			withServiceResultValue(json, function(data:Object){
					callback(data);
				},
				ignoreFailure,
				isObjP
			);
		}
		
		public function lookupFlvUrl(videoId:String, service:String, callback:Function):void{ 
			var params:Array = [["videoId", videoId],
				["serviceName", service]];
			dispatchServiceParamsPost(METH_FLV_URL, params, function(e:Event){
					parseLookupFlvUrlResponse(URLLoader(e.target).data, callback);
				});
		}
		private function parseLookupFlvUrlResponse(json:String, callback:Function):void{
			withServiceResultValue(json, function(data:Object){
					callback(data);
				},
				ignoreFailure,
				isObjP
			);
		}

		public function loadCanvas(callback:Function, failedCallback:Function):void{ 
			var params:Array = [["userId", user.userId]];
			dispatchServiceParamsPost(METH_LOAD_CANVAS, params, function(e:Event){
					parseLoadCanvasResponse(URLLoader(e.target).data, callback, failedCallback);
				});
		}
		private function parseLoadCanvasResponse(json:String, callback:Function, failedCallback:Function):void{
			withServiceResultValue(json, function(data){

						callback(CanvasModel.fromJson(data));
				},
				failedCallback,
				function(data):Boolean{ return isObjP(data) && data.id && data.groups }
			);
		}

		public function saveCanvas(callback:Function, failedCallback:Function){
			var req:Object = App.ref.canvasModel.toJson();
			dispatchServicePost(METH_SAVE_CANVAS, (new JSONEncoder(req)).getString(), function(e:Event){
					parseSaveCanvasResponse(URLLoader(e.target).data, callback, failedCallback);
				}, 
				failedCallback,
				"text/plain");
		}
		private function parseSaveCanvasResponse(json:String, callback:Function, failedCallback:Function):void{
			withServiceResultValue(json, function(result){
					callback(result);// This null result doesn't really tell us much..
				}, 
				failedCallback,
				isNullP 
			);
		}
		
		
		public function searchByTags(tags:String, resultsCallback:Function, failureCallback:Function = null, maxResults:int = 30):void{ 
			var params:Array = [["q", tags],
				["resultsPerPage", maxResults],
				["page", 1]
			];
			dispatchServiceParamsPost(METH_TAG_SEARCH, params, function(e:Event){
					parseSearchByTagsResponse(URLLoader(e.target).data, resultsCallback, failureCallback || function(){});
				});
		}
		private function parseSearchByTagsResponse(json:String, resultsCallback:Function, failureCallback:Function):void{
			withServiceResultValue(json, function(results:Object){
					if(results.videos){
						resultsCallback(VideoModel.manyFromJson(results.videos));
					}
				},
				failureCallback,
				isObjP
			);
		}
		

		/**
		* Get a set of 'fresh' or new videos.
		* 
		* @param resultsCallback 
		* @return 
		*/		
		public function getFreshVids(resultsCallback:Function):void{ 
			var params:Array = [["resultsPerPage", App.MAX_VIDEOS],
				["page", 1]];
			dispatchServiceParamsPost(METH_NEW_VIDEOS, params, function(e:Event){
					parseFreshVidsResponse(URLLoader(e.target).data, resultsCallback);
				});
		}
		private function parseFreshVidsResponse(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Object){
					if(results.videos){
						resultsCallback(VideoModel.manyFromJson(results.videos));
					}
				},
				ignoreFailure,
				isObjP
			);
		}
		
		

		/**
		* A helper that simply makes a search using vid's tags. A poor man's 
		* relevant results search.
		* 
		* @param vid 
		* @param callback 
		* @return 
		*/		
		public function getRelatedVids(vid:VideoModel, callback:Function):void{
			searchByTags(vid.tags, function(vids){
					callback(vids);
				});
		}
		

		public function getPopularVids(resultsCallback:Function):void{ 
			var params:Array = [["resultsPerPage", App.MAX_VIDEOS],
				["page", 1]
			];
			dispatchServiceParamsPost(METH_POPULAR_VIDEOS, params, function(e:Event){
					parsePopularVidsResponse(URLLoader(e.target).data, resultsCallback);
				});
		}


		private function parsePopularVidsResponse(json:String, resultsCallback:Function):void{
			withServiceResultValue(json, function(results:Object){
					if(results.videos){
						resultsCallback(VideoModel.manyFromJson(results.videos));
					}
				},
				ignoreFailure,
				isObjP
			);
		}

		public function checkForNewerVersion(newerFunc:Function, sameOrOlderFunc:Function):void{
			var loader:URLLoader = new URLLoader();
			loader.addEventListener( Event.COMPLETE, function(e:Event){
					withDecodedObject(URLLoader(e.target).data, function(result:Object){
							if(parseFloat(result.version) > parseFloat(App.ref.external.version)){
								newerFunc(result.version);
							}
							else{
								sameOrOlderFunc();
							}
						}, 
						sameOrOlderFunc
					);
				});
			loader.addEventListener( IOErrorEvent.IO_ERROR, function(e:IOErrorEvent){ 
					sameOrOlderFunc();
				});
			var req:URLRequest = new URLRequest(versionFileLocation);
			req.method = URLRequestMethod.GET;
			loader.load(req);
		}

		public function withBulletinMetadataDesc(callback:Function, failCallback:Function):void{
			var loader:URLLoader = new URLLoader();
			loader.addEventListener( Event.COMPLETE, function(e:Event){
					withDecodedArray(URLLoader(e.target).data, function(data:Array){
							callback(new EnumArray(data));
						}, 
						failCallback
					);
				});
			loader.addEventListener( IOErrorEvent.IO_ERROR, function(e:IOErrorEvent){ 
					failCallback();
				});
			var req:URLRequest = new URLRequest(bulletinMetadataUrl);
			req.method = URLRequestMethod.GET;
			loader.load(req);
		}
		
		/**
		* Helper to perform a GET request on uv service
		* 
		* @param method 
		* @param params 
		* @param callback 
		* @return 
		*/		
		private function dispatchServiceGet(method:String, params:Array, callback:Function){
			var url:String = jsonEndpoint + "?m=" + method + "&" + constructQueryString(params) + "&rand=" + Math.random();
			dispatchGet(url, callback);
		}

		/**
		* Helper to POST params to the uv service
		* 
		* @param method 
		* @param params 
		* @param callback 
		* @return 
		*/		
		private function dispatchServiceParamsPost(method:String, params:Array, callback:Function){
			var url:String = jsonEndpoint + "?m=" + method + "&rand=" + Math.random();
			dispatchPost(url, constructQueryString(params), callback);
		}

		/**
		* Helper to POST body to the uv service
		* 
		* @param method 
		* @param params 
		* @param callback 
		* @return 
		*/		
		private function dispatchServicePost(method:String, body, callback:Function, failedCallback:Function=null, contentType:String = null){
			var url:String = jsonEndpoint + "?m=" + method + "&rand=" + Math.random();
			dispatchPost(url, body, callback, failedCallback, contentType);
		}

		public function dispatchGet(url:String, callback:Function, failedCallback:Function=null){
			var req:URLRequest = new URLRequest(url);
			req.method = URLRequestMethod.GET;
			
			var loader:URLLoader = new URLLoader();
			var func_fail:Function = function(e) {
				e.target.removeEventListener( IOErrorEvent.IO_ERROR, arguments.callee );
				e.target.removeEventListener( Event.COMPLETE, func_complete );
				handleIoError(e, failedCallback);
			}
			var func_complete:Function = function(e) {
				e.target.removeEventListener( Event.COMPLETE, arguments.callee );
				e.target.removeEventListener( IOErrorEvent.IO_ERROR, func_fail );
				callback(e);
			}
			loader.load(req);
		}
		
		public function dispatchPost(url:String, body, callback:Function, failedCallback:Function = null, contentType:String = null) {
			var req:URLRequest = new URLRequest(url);
			req.method = URLRequestMethod.POST;
			if(contentType){ req.contentType = contentType; }
			req.data = body;
			
			var loader:URLLoader = new URLLoader();
			var func_fail:Function = function(e) {
				e.target.removeEventListener( IOErrorEvent.IO_ERROR, arguments.callee );
				e.target.removeEventListener( Event.COMPLETE, func_complete );
				handleIoError(e, failedCallback);
			}
			var func_complete:Function = function(e) {
				e.target.removeEventListener( Event.COMPLETE, arguments.callee );
				e.target.removeEventListener( IOErrorEvent.IO_ERROR, func_fail );
				callback(e);
			}
			loader.addEventListener (Event.COMPLETE, func_complete);
			loader.addEventListener( IOErrorEvent.IO_ERROR, func_fail);
			loader.load(req);
		}
		
		
		private function constructQueryString(params:Array){
			var keyVals:Array = [];
			for each(var pair:Array in params){
				keyVals.push(String(pair[0]) + "=" + String(pair[1]));
			}
			return keyVals.join("&");
		}
		
		private function handleIoError(e:IOErrorEvent, callback:Function=null){
			if ( callback != null ) {
				callback(e);
			}
		}

		/**
		* Given a json string, decode a service result S from the json.
		* Confirm that S.type indicates success, and that S.value conforms to valPredIn.
		* If everything looks good, pass S.value to ifFunc. Otherwise invoke elseFuncIn.
		*
		* TODO Perhaps an error indicator (or even S itself?) should be passed to the elseFuncIn callback?		
		* 
		* @param json 
		* @param ifFunc 
		* @param elseFuncIn 
		* @param valPredIn 
		*/		
		private function withServiceResultValue(json:String, ifFunc:Function, elseFuncIn:Function = null, valPredIn:Function = null):void{
			var elseFunc:Function = elseFuncIn || function(){}
			var valPred:Function = valPredIn || function(val):Boolean{ return true }
			withDecodedObject(json, function(result){

					var success:Boolean = result.type == SERVICE_RESULT_SUCCESS;
					var valP:Boolean;

					try{
						valP = valPred(result.value);
					}
					catch(e){
						valP = false;
					}

					if( success && valP){
						ifFunc(result.value);
					}
					else{
						if(success && !valP){
							trace("VALUE PREDICATE FAILED: " + json);
						}
						else if(!success){
							trace("RECEIVED ERROR RESULT: " + json);
						}
						elseFunc();
					}
				},
				elseFunc
			);
		}

		private function withDecodedObject(json:String, ifFunc:Function, elseFuncIn:Function = null):void{
			var elseFunc:Function = elseFuncIn || function(){};
			var obj;
			try{
				obj = (new JSONDecoder(json)).getValue();
			}
			catch(e){
				elseFunc();
				return;
			}

			if(obj is Object){
				ifFunc(obj);
			}
			else{
				elseFunc();
			}
		}

		private function withDecodedNull(json:String, ifFunc:Function, elseFuncIn:Function = null):void{
			var elseFunc:Function = elseFuncIn || function(){};
			var obj;
			try{
				obj = (new JSONDecoder(json)).getValue();
			}
			catch(e){
				elseFunc();
				return;
			}

			if(obj == null){
				ifFunc(obj);
			}
			else{
				elseFunc();
			}
		}

		private function withDecodedArray(json:String, ifFunc:Function, elseFuncIn:Function = null):void{
			var elseFunc:Function = elseFuncIn || function(){};
			var obj;
			try{
				obj = (new JSONDecoder(json)).getValue();
			}
			catch(e){
				elseFunc();
				return;
			}

			if(obj is Array){
				ifFunc(obj);
			}
			else{
				elseFunc();
			}
		}

		private function withDecodedXML(data:String, ifFunc:Function, elseFuncIn:Function = null):void{
			var elseFunc:Function = elseFuncIn || function(){};
			var obj;
			try{
				obj = new XML(data);
			}
			catch(e){
				elseFunc();
				return;
			}

			if(obj is XML){
				ifFunc(obj);
			}
			else{
				elseFunc();
			}
		}

		private function ignoreFailure():void {}
		private function isObjP(val):Boolean { return val is Object }
		private function isNullP(val):Boolean { return val == null }
		private function isArrayP(val):Boolean { return val is Array }
		
	}
}


