#!/bin/bash

# PERT Canvas Development Startup Script
echo "🚀 Starting PERT Canvas development environment..."

# Check if .secret file exists
if [ ! -f ".secret" ]; then
    echo "⚠️  Warning: .secret file not found. Backend authentication will fail."
    echo "   Create .secret with your Plan.io credentials to enable API integration."
fi

# Function to kill background processes on script exit
cleanup() {
    echo
    echo "🛑 Shutting down development servers..."
    if [ ! -z "$BACKEND_PID" ]; then
        kill $BACKEND_PID 2>/dev/null
        echo "   Backend server stopped"
    fi
    if [ ! -z "$FRONTEND_PID" ]; then
        kill $FRONTEND_PID 2>/dev/null
        echo "   Frontend server stopped"
    fi
    echo "✅ All servers stopped"
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

echo
echo "📦 Installing backend dependencies..."
clj -P

echo
echo "🔧 Starting backend server (port 3000)..."
clj -M:run &
BACKEND_PID=$!

# Give backend a moment to start
sleep 2

echo
echo "⚛️  Starting frontend development server (port 8080)..."
npx shadow-cljs watch frontend &
FRONTEND_PID=$!

echo
echo "✅ Development environment started!"
echo
echo "🌐 Frontend: http://localhost:8080"
echo "🔌 Backend:  http://localhost:3000"
echo "🔧 NREPL:    localhost:7002"
echo
echo "📝 Logs will appear below. Press Ctrl+C to stop all servers."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Wait for both processes
wait