const formElem = document.getElementsByTagName("form")[0]; 
const submitInput = document.getElementById("submit");
const selectFileInput = document.getElementById("select_file");


formElem.reset();
submitInput.disabled = true;

formElem.formdata = function() {
    submitInput.disabled = false;
}

selectFileInput.onchange = function(){
    submitInput.disabled = false;
}