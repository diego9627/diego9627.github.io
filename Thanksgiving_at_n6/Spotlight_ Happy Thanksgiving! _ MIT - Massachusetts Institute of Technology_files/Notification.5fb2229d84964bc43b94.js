webpackJsonp([5],{386:function(t,n,e){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var i=e(410),o=e.n(i);for(var a in i)"default"!==a&&function(t){e.d(n,t,function(){return i[t]})}(a);var s=e(39),u=Object(s.a)(o.a,void 0,void 0,!1,null,null,null);u.options.__file="src/js/vue-components/Notification.vue",n.default=u.exports},410:function(t,n,e){"use strict";(function(t){Object.defineProperty(n,"__esModule",{value:!0});var i=function(t){return t&&t.__esModule?t:{default:t}}(e(431));n.default={el:"#notification",data:{isHomepage:!1},components:{notification:i.default},mounted:function(){t("body").hasClass("front")&&(this.isHomepage=!0)}}}).call(n,e(18))},411:function(t,n,e){"use strict";(function(t){Object.defineProperty(n,"__esModule",{value:!0}),n.default={data:function(){return{messages:[],display:!1}},mounted:function(){var n=this;t.getJSON("/spotlights-export/announcements/?_format=json").done(function(e){n.messages=e,0!=e.length&&(t("body").addClass("has-notification"),n.display=!0)})},computed:{notificationClass:function(){var t=!1;return this.messages.forEach(function(n){"e"==n.type&&(t=!0)}),t?"notification__mod notification--emergency theme--emergency":"notification__mod notification--announcement"}}}}).call(n,e(18))},431:function(t,n,e){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var i=e(411),o=e.n(i);for(var a in i)"default"!==a&&function(t){e.d(n,t,function(){return i[t]})}(a);var s=e(432),u=e(39),c=Object(u.a)(o.a,s.a,s.b,!1,null,null,null);c.options.__file="src/js/vue-components/NotificationMessage.vue",n.default=c.exports},432:function(t,n,e){"use strict";e.d(n,"a",function(){return i}),e.d(n,"b",function(){return o});var i=function(){var t=this,n=t.$createElement,e=t._self._c||n;return t.display?e("div",{class:t.notificationClass},[e("div",{staticClass:"notification__width"},[e("h2",{staticClass:"notification__h"},[t._v("Important")]),t._v(" "),e("div",{staticClass:"notification__text-mod"},t._l(t.messages,function(n,i){return e("div",{key:i},[e("div",{domProps:{innerHTML:t._s(n.message)}})])}))])]):t._e()},o=[];i._withStripped=!0}});