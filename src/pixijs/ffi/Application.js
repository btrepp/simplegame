
exports.newApplicationImpl = function (arg){
    return function(){
        return new PIXI.Application(arg)
    }
}

exports.viewImpl = function (application){
    return application.view;
}