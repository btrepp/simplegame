
exports.blobUriImpl = function (blob){
    return function (){
        return URL.createObjectURL(blob);
    }
}