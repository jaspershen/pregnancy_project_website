/**
 * author： 谯杨
 * published： 2012.08.07
 */

/**
 * @param id 获取元素的id
 * @return id所对应的结点
 */
var atuokey = false;

function $(id) { 
    return document.getElementById(id); 
}

function loadFunction(func){
    var oldonload = window.onload;
    if (typeof window.onload != 'function') {
        window.onload = func;
    } else {
        window.onload = function(){
            oldonload();
            func();
        }
    }
}

//移动元素的函数
function moveElement(elementId, final_x, final_y, interval){
    node = $(elementId);

    if(node.movement){
        clearTimeout(node.movement);
    }
    if(!node.style.left){
        node.style.left = "0px";
    }
    if(!node.style.top){
        node.style.top = "0px";
    }

    var xpos = parseInt(node.style.left);
    var ypos = parseInt(node.style.top);
    if(xpos == final_x && ypos == final_y){
        return true;
    }
    if(xpos < final_x){
        var dis = Math.ceil((final_x - xpos)/10)
        xpos += dis;
    }
    if(xpos > final_x){
        var dis = Math.ceil((xpos-final_x)/10);
        xpos -= dis;
    }
    if(ypos < final_y){
        var dis = Math.ceil((final_y - ypos)/10);
        ypos += dis;
    }
    if(ypos > final_y){
        var dis = Math.ceil((ypos - final_y) /10);
        ypos -= dis;
    }

    node.style.left = xpos + "px";
    node.style.top = ypos + "px";

    var repeat = "moveElement('"+elementId+"',"+final_x+","+final_y+","+interval+")";
    node.movement = setInterval(repeat,interval);
}

function classNormal(picFoucusId, picTextId){
    var focusNodes = $(picFoucusId).getElementsByTagName("li");
    var textNodes = $(picTextId).getElementsByTagName("li");
    for(var i = 0; i < 4; i++ ){
        focusNodes[i].className = "normal";
        textNodes[i].className = "normal";
    }
}

function classCurrent(picFoucusId, picTextId, n){
    var focusNodes = $(picFoucusId).getElementsByTagName("li");
    var textNodes = $(picTextId).getElementsByTagName("li");
    focusNodes[n].className = "current";
    textNodes[n].className = "current";
}

function picSwitch(){
    if(!$("picWhole")) return false;
    $("picWhole").onmouseover = function(){atuokey = true};
    $("picWhole").onmouseout = function(){atuokey = false};
    var picFocus = $("picFoucus").getElementsByTagName('li');
    var listLength = picFocus.length;
    if(listLength == 4){
        picFocus[0].onmouseover = function() {
                moveElement('picList',0,0,10);
                classNormal('picFoucus','picText');
                classCurrent('picFoucus','picText',0);
            }
            picFocus[1].onmouseover = function() {
                    moveElement('picList',0,-225,10);
                    classNormal('picFoucus','picText');
                    classCurrent('picFoucus','picText',1);
            }
            picFocus[2].onmouseover = function() {
                moveElement("picList", 0, -450, 10);
                classNormal('picFoucus','picText');
                classCurrent('picFoucus','picText',2);
            }
            picFocus[3].onmouseover = function() {
                moveElement('picList',0,-675,10);
                classNormal('picFoucus','picText');
                classCurrent('picFoucus','picText',3);
            }
    }
}

function operation(n){
    var x = [0, -225, -450, -675];
    if(n == 3){
        moveElement('picList',0,x[n-3],10);
        classNormal('picFoucus','picText');
        classCurrent('picFoucus','picText',0);
    }else{
        moveElement('picList',0,x[n+1],10);
        classNormal('picFoucus','picText');
        classCurrent('picFoucus','picText',n+1);
    }    
}

function autoSwitch() {
    if(!$('picWhole')) return false;
    if(atuokey) return false;
    var picFocus = $('picFoucus').getElementsByTagName('li');
    var listLength = picFocus.length;
    var currentNum = -1;
    for(var i=0; i<listLength; i++) {
        if (picFocus[i].className == 'current') {
                currentNum = i;
                break;
        }
    }
    operation(currentNum);
}
setInterval("autoSwitch()", 3000);
loadFunction(picSwitch);