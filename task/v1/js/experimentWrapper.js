// function for navigating experiment stages
function updateMainMenu(expStage){
  // display text based on experiment stage
  switch(expStage){
    case 1: //initial sound check
      $("#table").hide();
      $("#soundCheck").show();
      break;
    case 2: // demographics
      $("#soundCheck").hide();
      $("#demographicsForm").show();
      break;
    case 3: // STAI questionnaire
      $("#demographicsForm").hide();
      $("#soundCheck").hide();
      $("#table").hide();
      $("#STAI_div").show();
      break;
    case 4: //main task
      $("#demographicsForm").hide();
      $("#soundCheck").hide();
      $("#table").hide();
      $("#STAI_div").hide();
      startExperiment();
      break;
    case 5: //debriefing
      $("#mturk_form").show();
      $("#demographicsForm").hide();
      $("#soundCheck").hide();
      $("#STAI_div").hide();
      $("#table").show();
      break;
  }
}

$(document).ready(function(){
  // initial hide all DOM elements
  $("#table").hide();
  $("#soundCheck").hide();
  $("#mturk_form").hide();
  $("#instructions").hide();
  $("#myButton").hide();
  $("#NoGo").hide();
  $("#STAI_div").hide();

  // gets MTurk Worker Information and assign to HTML elements
  let mt = getAllUrlParams(window.location.href);

  document.getElementById('assignmentId').value = mt.assid;
  document.getElementById('hitId').value = mt.hitid;
  document.getElementById('workerId').value = mt.workerid;

  console.log('assid: ', document.getElementById('assignmentId').value);
  console.log('hitid: ',document.getElementById('hitId').value);
  console.log('workerid: ',document.getElementById('workerId').value);

  // prevents multiple soundcheck checkboxes being selected
  $('input[type="checkbox"]').on('change', function(){
     $('input[type="checkbox"]').not(this).prop('checked', false);
  });

  // check worker ID
  if (document.getElementById("assignmentId").value == "" || document.getElementById("assignmentId").value == "ASSIGNMENT_ID_NOT_AVAILABLE" || document.getElementById("assignmentId").value == "assignment_id_not_available"){

    // display text for accepting HIT
    $("#table").show();
    $("#instruction").text("Accept HIT first");
    $("#instruction").show();
    $("#redo").hide();

  } else {

    prepareMenu();

  }

  let input_array = document.getElementById('STAI').querySelectorAll('input');
  for (let inputelement = 0; inputelement < input_array.length; inputelement++) {
    let p_element = input_array[inputelement].parentElement;
    p_element.onclick = function(){
      input_array[inputelement].checked = true;
    }
  };
});

function prepareMenu(){
  // update menu to first value
  let initialExperimentStage = 2 //start at demographics, no sound check needed
  updateMainMenu(initialExperimentStage);

  $("#demographicSubmit").click(function(){
    let anyBlank = false;
    if (document.getElementById("gender").value == "Blank") {
        document.getElementById("gender").style.borderColor = "red";
        anyBlank = true;
    }
    if (document.getElementById("ethnicity").value == "Blank") {
        document.getElementById("ethnicity").style.borderColor = "red";
        anyBlank = true;
    }
    if (document.getElementById("race").value == "Blank") {
        document.getElementById("race").style.borderColor = "red";
        anyBlank = true;
    }
    if (!anyBlank) {
      let demoData = [];
      let e = document.getElementById("gender");
      demoData.push(e.options[e.selectedIndex].value);
      demoData.push(document.getElementById("age").value);
      e = document.getElementById("ethnicity");
      demoData.push(e.options[e.selectedIndex].value);
      e = document.getElementById("race");
      demoData.push(e.options[e.selectedIndex].value);
      console.log(demoData);
      $("#demographics").val(demoData.join(";"));
      updateMainMenu(3);
    }
  });

  $("#soundCheckSubmit").click(function(){
    if (document.getElementById("sc2").checked == true) {
      updateMainMenu(2);
    } else {
      window.alert("Incorrect. Please try again.");
    }
  });
}

var STAI_vals = {}
$(document).on("click", "#STAI_submit", function(){

    STAI_vals = {}

    // code for submit button here
    let input_array = document.getElementById('STAI').querySelectorAll('input');

    for (let inputelement = 0; inputelement < input_array.length; inputelement++) {
      if (input_array[inputelement].checked) {

        STAI_vals[input_array[inputelement].name] = input_array[inputelement].value;

      }

    };

    console.log(STAI_vals);
    // console.log(Object.keys(STAI_vals).length);
    // console.log(Object.keys(STAI_vals));


//if participant answers all questions
if (Object.keys(STAI_vals).length == 20) {

  // const questions_list_array = document.getElementsByClassName("STAI_question");
  //   for (let i = 1; i < questions_list_array.length+1; i++) {
  //     console.log("question"+i);
  //     var questionInArray = document.getElementById("STAI_question"+i);
  //     questionInArray.style.color = 'black'
  //   }
  $("#STAI_data").val(Object.values(STAI_vals).join(';'));
  updateMainMenu(4);

}

//if participant does not answer all questions/skips question(s)
else if (Object.keys(STAI_vals).length < 20) {

  alert("Please answer all questions.");
  let questions_list_array = document.getElementsByClassName("STAI_question");
  for (let i = 1; i < questions_list_array.length+1; i++) {
    let questionInArray = document.getElementById("STAI_question"+i);
    questionInArray.style.color = ("question"+i in STAI_vals) ? 'black' : 'red';
  }

}
});

function demoChange(event){
  if (document.getElementById("gender").value != "Blank") {
      document.getElementById("gender").style.borderColor = "black";
  }
  if (document.getElementById("ethnicity").value != "Blank") {
      document.getElementById("ethnicity").style.borderColor = "black";
  }
  if (document.getElementById("race").value != "Blank") {
      document.getElementById("race").style.borderColor = "black";
  }
}

function getAllUrlParams(url) {

    // get query string from url (optional) or window
    var queryString = url ? url.split('?')[1] : window.location.search.slice(1);

    // we'll store the parameters here
    var obj = {};

    // if query string exists
    if (queryString) {

        // stuff after # is not part of query string, so get rid of it
        queryString = queryString.split('#')[0];

        // split our query string into its component parts
        var arr = queryString.split('&');

        for (var i = 0; i < arr.length; i++) {
            // separate the keys and the values
            var a = arr[i].split('=');

            // set parameter name and value (use 'true' if empty)
            var paramName = a[0];
            var paramValue = typeof (a[1]) === 'undefined' ? true : a[1];

            // (optional) keep case consistent
            paramName = paramName.toLowerCase();
            if (typeof paramValue === 'string') paramValue = paramValue.toLowerCase();

            // if the paramName ends with square brackets, e.g. colors[] or colors[2]
            if (paramName.match(/\[(\d+)?\]$/)) {

                // create key if it doesn't exist
                var key = paramName.replace(/\[(\d+)?\]/, '');
                if (!obj[key]) obj[key] = [];

                // if it's an indexed array e.g. colors[2]
                if (paramName.match(/\[\d+\]$/)) {
                    // get the index value and add the entry at the appropriate position
                    var index = /\[(\d+)\]/.exec(paramName)[1];
                    obj[key][index] = paramValue;
                } else {
                    // otherwise add the value to the end of the array
                    obj[key].push(paramValue);
                }
            } else {
                // we're dealing with a string
                if (!obj[paramName]) {
                    // if it doesn't exist, create property
                    obj[paramName] = paramValue;
                } else if (obj[paramName] && typeof obj[paramName] === 'string') {
                    // if property does exist and it's a string, convert it to an array
                    obj[paramName] = [obj[paramName]];
                    obj[paramName].push(paramValue);
                } else {
                    // otherwise add the property
                    obj[paramName].push(paramValue);
                }
            }
        }
    }

    return obj;
}
