# PERT Canvas

An interactive web application for creating and visualizing PERT (Program Evaluation and Review Technique) diagrams from task lists with dependencies.

## Demo

[Live Demo](https://nigelharsch.com/pert-canvas)

## Overview

PERT Canvas helps project managers and teams visualize critical paths in complex projects by converting task lists into interactive network diagrams. The application automatically calculates and highlights the critical path, making it easy to identify bottlenecks and optimize project timelines.

## Features

- **Interactive Diagram Creation**: Build PERT charts from CSV task lists or manual input
- **Critical Path Analysis**: Automatically calculates and highlights the longest path through your project
- **Drag-and-Drop Interface**: Intuitive node positioning and relationship management
- **Task Dependencies**: Visual representation of task relationships and dependencies
- **Data Import/Export**: Support for CSV file import with task data
- **Real-time Updates**: Dynamic recalculation of critical paths as tasks change


## Usage

1. **Import Tasks**: Upload a CSV file with your task data or use the example file (`example_tasks.csv`)
2. **Define Dependencies**: Specify task relationships using task IDs
3. **Generate Diagram**: The application automatically creates a visual PERT chart
4. **Analyze Critical Path**: View highlighted critical path and task timing information
5. **Customize Layout**: Drag nodes to optimize the visual presentation

### CSV Format

Your CSV file should include the following columns:
- **ID**: Unique identifier for each task
- **Title**: Task name or short description
- **Description**: Detailed task information (optional)
- **Related**: Comma-separated list of dependent task IDs
- **Due Date**: Task deadline (optional)

Example:
```csv
ID,Title,Description,Related,Due Date
432,Write Unit tests,,466,
455,Upgrade HTTP lib,,,
475,Deliver approved designs,,,
488,Build FE components,,"466, 475",
466,Build BE API,,455,
```

## Technology Stack

- **ClojureScript**: Functional programming language for robust application logic
- **Malli**: Data validation and schema definition
- **ReactFlow**: Interactive node-based diagram rendering
- **Material-UI DataGrid**: Data table components for task management
- **Shadow-CLJS**: ClojureScript build tool and development environment

## License

This project is open source and available under the [MIT License](LICENSE).
