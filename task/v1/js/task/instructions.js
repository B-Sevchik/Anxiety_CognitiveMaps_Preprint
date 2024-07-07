// see function navigateInstructionPath() in tasks.js for navigation code

// global instruction iterator information. Change as needed
let instructions = {
  // contains the interator for each instruction block
  iterator: {
    "main1-1": 1, "main1-2": 1, "main2": 1, "main3": 1, "main4": 1, "final": 1
  },
  // contains the max value of each instruction iteration. iteration will STOP at max.
  max: {
    "main1-1": 4, "main1-2": 7, "main2": 7, "main3": 4, "main4": 8, "final": 1
  },
  // what does instruction section end with?
  // #nextSectionButton, #startExpButton, keyPressNextSection, keyPressStartTask
  exitResponse: {
    "main1-1": '#nextSectionButton',
    "main1-2": '#startExpButton',
    "main2": 'keyPressStartTask',
    "main3": 'keyPressStartTask',
    "main4": '#startExpButton',
    "final": 'keyPressStartTask'
  }
};
let iterateAgain = false, task;

function navigateInstructionPath(repeat = false){
  if (repeat == true) {
    // if multi stage instructions, ensures it goes back to first not second
    // switch (expStage){
    //   case "prac1-1":
    //   case "prac1-2":
    //     expStage = "prac1-1";
    //     break;
    // }
  } else {
    // what is the sequence of experimental instruction stages?
    switch (expStage){
      case "main1-1":
        expStage = "main1-2";
        break;
      case "main1-2":
        expStage = "main2";
        break;
      case "main2":
        expStage = "main3";
        break;
      case "main3":
        expStage = "main4";
        break;
      case "main4":
        expStage = "final";
        break;
    }
  }
  runInstructions();
}

function displayDefaults(stage){
  // default values of instruction blocks. add any special cases
  switch(stage){
    case "final":
      showFirst();
      $('.instruction-header').hide();
      break;
    default:
      showFirst();
      $('.instruction-header').show();
      break;
  }
}

function getNextInstructions(slideNum, expStage){
/* use the following options when modifying text appearance
    -  iterateAgain = true;
    -  changeTextFormat('#instructions' + slideNum,'font-weight','bold');
    -  changeTextFormat('#instructions' + slideNum,'font-size','60px');
    -  changeTextFormat('#instructions' + slideNum,'color','red');
    -  changeTextFormat('#instructions' + slideNum,'margin-top', '20px');
    -  changeTextFormat('#instructions' + slideNum,'margin-bottom', '20px');
    - $("<img src='../pics/finalpics/M33.jpg' class='insertedImage'>").insertAfter( "#instructions" + slideNum);
    - $( getImageText(instructionImages[1])).insertAfter( "#instructions" + slideNum);
*/
  switch (expStage){
    case "main1-1":
      switch (slideNum){
        case 1:
          return "Welcome to the experiment, thank you for your participation!";
        case 2:
          return "In this experiment, you will perform a task where you learn a network structure of images. You will then perform two follow up tasks designed to test your knowledge of the network structure. The experiment is expected to take approximately 30 to 40 minutes.";
        case 3:
          return "Please enlarge this window to its maximum size and sit a comfortable distance from your computer screen.";
        case 4:
          return "Please also remember to respond as quickly and as accurately as possible during the tasks.";
      }
    case "main1-2":
      switch (slideNum){
        case 1:
          return "In this first task, you will memorize the locations of some images within a network structure.";
        case 2:
          return "You will see an empty network structure along with a bank of images.";
        case 3:
          return "Drag & drop each image from the image bank into the slots of the network structure. At first you will just be guessing. Press 'check answer' when you are finished filling in the network structure.";
        case 4:
          return "If an image is in the correct location, the box it is in will be outlined in green. If the image is in the wrong location, the box will be outlined in red.";
        case 5:
          return "If an image is incorrect, drag and drop it to replace it with other incorrect images, and press 'check answer' again. Continue guessing and checking until all the images are in the correct locations.";
        case 6:
          return "<b>You will repeat the above process until you have learned the network structure, by placing all images correctly on the first attempt 3 consecutive times.</b> The task is expected to take approximately 10 minutes, but varies depending on your accuracy.";
        case 7:
          return "Press the Start Experiment button when you are ready to begin the task.";
      }
    case "main2":
      switch (slideNum){
        case 1:
          return "Great job! You will now complete task 2 based on your knowledge of the network structure."
        case 2:
          $( getImageText(instructionImages[3])).insertAfter( "#instructions" + slideNum);
          changeTextFormat('#instructions' + slideNum,'margin-bottom', '5px');
          return "Jack and Jill are playing a game where they take turns choosing a picture from the network you just memorized. They are only allowed to pick a picture that is adjacent to the previous picture. A picture is considered adjacent to another picture if a line connects them. For example, in the diagram below, if the current picture is the 'lightning bolt', they would only be allowed to pick 'pine cones' or 'pig', not 'yarn', since there is no line connecting 'lightning bolt' and 'yarn'.";
        case 3:
          return "Imagine you are Jack and Jill's caretaker, and you want to make sure they are playing fairly. Jill loves to cheat. Sometimes, she will choose a picture that is NOT adjacent to the previous picture. Your job is to catch whenever she tries to cheat.";
        case 4:
          return "In this next task, you will see a series of images from the network structure you just memorized. Press 'z' whenever Jill makes a correct move and 'm' whenever she cheats.";
        case 5:
          return "You will start with a practice task, and you will receive feedback on every trial. You need at least 80% accuracy to proceed to the main task."
        case 6:
          iterateAgain = true;
          $( getImageText(instructionImages[4])).insertAfter( "#instructions" + slideNum);
          return "Please place your fingers on the 'z' and 'm' keys before beginning the task. Remember to respond as quickly and as accurately as possible."
        case 7:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Press any button to begin the task."
      }
    case "main3":
      switch (slideNum){
        case 1:
          return "You will now begin the main task. Remember, Jack and Jill may only choose images that are adjacent to the last image, and it is your job to catch Jill whenever she cheats and chooses a non-adjacent image.";
        case 2:
          return "For this task, you only need to respond if and when Jill cheats, i.e., when an image appears that was not adjacent to the previous image in the task sequence. Press 'space bar' whenever this occurs, otherwise press nothing at all.";
        case 3:
          return "You will get feedback whenever you accuse Jill of cheating when she wasn't, or if you accidentally miss an instance when Jill did cheat. Otherwise, the task will proceed uninterrupted. This task is expected to take 10 minutes.";
        case 4:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Press the space bar to begin the task."
    }
    case "main4":
      switch (slideNum){
        case 1:
          return "Great job! You will now be tested on your memory of the network structure.";
        case 2:
          return "In this task, you will use your knowledge of the network structure that you memorized. You may remember that the images in the network structure belong to two distinct groups or communities, one on the left and one on the right.";
        case 3:
          return "In this task, you will see three images. Two of these images will belong to one of the communities, and the third image will belong to the other community.";
        case 4:
          return "Your job is to choose the image that belongs to a different community than the other two images. For example, if you think that the first and third image were in the same community, choose the second image.";
        case 5:
          changeTextFormat('#instructions' + slideNum,'font-weight','bold');
          return "Do not choose based on what the images look like. Your choice should be based purely on the position of the images in the network structure.";
        case 6:
          return "Note that the position of the images in the three locations is random and should not influence your decision.";
        case 7:
          return "Please take your time and think carefully about which image does not fit with the other two. If you are unsure, make your best guess.";
        case 8:
          return "This task is expected to take 10 minutes.";
    }
    case "final":
      switch (slideNum){
        case 1:
          experimentFlow();
          break;
      }
  }
}

function runInstructions(){
  // show cursor
  document.body.style.cursor = 'auto';

  // main instruction function (come here at start of instruction block)
  sectionStart = new Date().getTime() - runStart;
  sectionType = "instructions";

  // hide/clear everything, just in case
  hideInstructions();

  // hide canvas and other tasks if visible
  $(".canvasas").hide();
  $("#oddOneOutTaskDiv").hide();
  $("#network-diagram").hide();
  if (showNavButtons) {
    $("#navButtons").show();
  }

  // if need to repeat instructions (e.g., participant failed to meet accuracy requirement), then reshow all instructions
  if (instructions["iterator"][expStage] >= instructions["max"][expStage]){

    // loop through instructions and show
    for (var i = 1; i <= instructions["max"][expStage]; i++) {
      $('#instructions' + i).html( getNextInstructions( i, expStage ));
    }

    // reset iterateAgain in case looping turned it on by accident
    iterateAgain = false;

    // display instructions and prepare exit response mapping
    $('#instructionsDiv').show();
    displayDefaults(expStage);
    exitResponse();

  } else {

    // remove any previous click listeners, if any
    $(document).off("click","#nextInstrButton");
    $(document).off("click","#startExpButton");
    $(document).off("click","#nextSectionButton");

    // clear all previous instructions, reset styles, and remove pictures
    for (let i = 1; i <= 8; i++) {
      $('#instructions' + i).text("");
      resetDefaultStyles('#instructions' + i);
      clearInsertedContent();
    }

    // display proper instruction components, in case they are hidden
    $('#instructionsDiv').show();
    $('#nextInstrButton').show();
    $('#nextSectionButton').hide();
    $('#startExpButton').hide();
    displayDefaults(expStage);
  }

  /* code for "Next" button click during instruction display
        if running from Atom, need to use $(document).on, if through Duke Public Home Directory, either works.
        https://stackoverflow.com/questions/19237235/jquery-button-click-event-not-firing
  */
  $(document).on("click", "#nextInstrButton", function(){
  // $("#nextInstrButton").on('click', function(){
    iterateInstruction();
  });

  // code for click startExperiment button
  $(document).on('click', '#startExpButton', function(){
    //update data logger on time spent in section
    sectionEnd = new Date().getTime() - runStart;
    logSectionData();

    $('#instructionsDiv').hide();
    $('#startExpButton').hide();
    clearInsertedContent();

    // clear all button press listeners
    $(document).off("click","#nextInstrButton");
    $(document).off("click","#startExpButton");
    $(document).off("click","#nextSectionButton");
    experimentFlow();
  });

  $(document).on('click', '#nextSectionButton', function(){
    // update data logger on time spent in section
    sectionEnd = new Date().getTime() - runStart;
    logSectionData();

    // clear all button press listeners
    $(document).off("click","#nextInstrButton");
    $(document).off("click","#startExpButton");
    $(document).off("click","#nextSectionButton");
    navigateInstructionPath();
  });
};

function iterateInstruction(){
  let instrNum = instructions["iterator"][expStage];
  $('#instructions' + instrNum).html( getNextInstructions( instrNum, expStage));

  // iterate as appropriate or allow next phase
  if (instrNum < instructions["max"][expStage]){
    instructions["iterator"][expStage]++;
  } else{
    exitResponse();
  }

  if (iterateAgain == true) {
    iterateAgain = false;
    iterateInstruction();
  }
}

function exitResponse(){
  $('#nextInstrButton').hide();
  if (instructions["exitResponse"][expStage] == "#startExpButton"){
    $('#startExpButton').show();
  } else if (instructions["exitResponse"][expStage] == "#nextSectionButton") {
    $('#nextSectionButton').show();
  } else if (instructions["exitResponse"][expStage] == "keyPressStartTask"){
    keyListener = 5;
  } //else if (instructions["exitResponse"][expStage] == "buttonPressNextSection"){
    //keyListener = 6;
  //}
}

function getImageText(imageURL){
  return "<img src='" + imageURL + "' class='insertedImage'>";
}

function showFirst() {
  iterateInstruction();
}

function changeTextFormat(elementName, property ,changeTo){
  $(elementName).css( property , changeTo );
}

function clearInsertedContent(){
  $('.insertedImage').remove();
  $('.insertedContent').remove();
}

function hideInstructions(){
  // remove any previous click listeners, if any
  $(document).off("click","#nextInstrButton");
  $(document).off("click","#startExpButton");
  $(document).off("click","#nextSectionButton");

  // hide instruction DOMs
  $('#instructionsDiv').hide();
  $('#startExpButton').hide();
  $('#nextSectionButton').hide();

  // clear text from instruction DOMs
  for (let i = 1; i <= 8; i++) {
    $('#instructions' + i).text("");
    resetDefaultStyles('#instructions' + i);
    clearInsertedContent();
  }
}

function resetDefaultStyles(domObject){
  $(domObject).css('font-weight','');
  $(domObject).css('font-size','');
  $(domObject).css('color','');
  $(domObject).css('margin-top','');
  $(domObject).css('margin-bottom','');
}

function getKeyMappingText(item){
  if (keyMapping == 1 ? item == 2 : item == 1) {
    return "is oriented correctly.";
  } else {
    return "has been rotated.";
  }
}
