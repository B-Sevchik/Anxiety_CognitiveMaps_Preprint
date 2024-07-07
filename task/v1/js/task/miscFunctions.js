
// Fisher-Yates shuffle
function shuffle(array){
  for(let j, x, i = array.length; i; j = Math.floor(Math.random() * i), x = array[--i], array[i] = array[j], array[j] = x);
  return array;
}

function fileOnly(strSRC){
  return strSRC.match(/[^\\/:*?"<>|\r\n]+$/g)[0];
}

function randIntFromInterval(min, max) { // min and max included
  return Math.floor(Math.random() * (max - min + 1) + min);
}

function multinomialSample(sampleArr, probArr){
  // build probability integer var
  let sampleProbs = [];
  let probability = 0;
  for (let i = 0; i < sampleArr.length; i++) {
    probability += probArr[i];
    sampleProbs.push(probability);
  }

  // get random number, use to grab value
  let randNumber = Math.random();
  for (let i = 0; i < sampleProbs.length; i++) {
    if (randNumber > sampleProbs[i]) {
      continue;
    } else {
      return sampleArr[i];
    }
  }
}

function hideCursor(){
  document.body.style.cursor = 'none';
}

function displayCanvases(){
  canvas.style.display = "inline-block";
  if (showNetworkWalk == true) {ntCanvas.style.display = "inline-block";}
  $(".canvasas").show();
}

function setUpCanvases(){
  // set up main display canvas
  canvas = document.getElementById('taskCanvas');
  ctx = canvas.getContext('2d');
  ctx.textBaseline= "middle";
  ctx.textAlign="center";

  // set up canvas for showing network walk
  ntCanvas = document.getElementById('networkCanvas');
  ntCtx = networkCanvas.getContext('2d');
  ntCtx.textBaseline= "middle";
  ntCtx.textAlign="center";
}
