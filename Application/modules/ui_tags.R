
# SPA Application UI Tags

library(shiny)

logo <- tags$a(
  tags$img(
    src = "logo.png",
    style = "position: absolute; right: 10px; top: 5px; z-index: 1000; padding: 5px; margin-bottom: 20px;",
    width = "80"
  ),
  ""
)

# Custom CSS for Tertiary Button ------------------------------------------
custom_css <- tags$head(
  tags$style(HTML("
    /* General button styles */
    .btn-primary, .btn-secondary, .btn-save, .btn-trust, .btn-danger {
      background-color: rgba(12, 61, 18, 0.2);  /* Light transparent green */
      color: black;  /* Default text color */
      border: 2px solid transparent;  /* Add a border */
      transition: background-color 0.3s, color 0.3s, border-color 0.3s;  /* Smooth transition */
    }

    /* Default border colors */
    .btn-primary {
      border-color: rgba(12, 61, 18, 0.3); /* Light green border */
    }
    /* General Modal Button Styling */
    .modal-footer .btn-default {
      background-color: rgba(12, 61, 18, 0.2); /* Light transparent dark red */
      color: black;
      border: 2px solid rgba(12, 61, 18, 0.3);
      transition: background-color 0.3s, color 0.3s, border-color 0.3s;
    }
    .modal-footer .btn-default:hover {
      background-color: #0C3D12; /* Solid dark red on hover */
      color: white;
      border-color: #0C3D12;
    }
     /* Modal Header Styling */
    .modal-header {
      background-color: #f0f0f0; /* Light grey */
      color: black; /* Text color */
      border-bottom: 1px solid #ddd; /* Optional: Border for separation */
    }
    /* Primary button styling for Browse */
    .btn-file {
      background-color: rgba(12, 61, 18, 0.2); /* Match primary color */
      color: black;
      border: 2px solid rgba(12, 61, 18, 0.3); /* Match border */
      transition: background-color 0.3s, color 0.3s, border-color 0.3s;
    }
    .btn-file:hover {
      background-color: #0C3D12; /* Match hover color for primary */
      color: white;
      border-color: #0C3D12;
    }
    .btn-secondary {
      background-color: rgba(126, 52, 52, 0.2); /* Light transparent red */
      border-color: rgba(126, 52, 52, 0.3); /* Light red border */
    }
    .btn-save {
      background-color: rgba(0, 64, 128, 0.2); /* Light transparent blue */
      border-color: rgba(0, 64, 128, 0.3); /* Light blue border */
    }
    .btn-danger {
      background-color: rgba(139, 0, 0, 0.2); /* Light transparent dark red */
      border-color: rgba(139, 0, 0, 0.3); /* Light dark red border */
    }

    /* Hover styles */
    .btn-primary:hover {
      background-color: #0C3D12; /* Company green */
      color: white;
      border-color: #0C3D12; /* Green border */
    }
    .btn-secondary:hover {
      background-color: #7E3434; /* Brick red */
      color: white;
      border-color: #7E3434; /* Red border */
    }
    .btn-save:hover, .btn-trust:hover {
      background-color: #004080; /* Dark blue */
      color: white;
      border-color: #004080; /* Blue border */
    }
    .btn-danger:hover {
      background-color: #8B0000; /* Dark red */
      color: white;
      border-color: #8B0000; /* Red border */
    }
  "))
)
