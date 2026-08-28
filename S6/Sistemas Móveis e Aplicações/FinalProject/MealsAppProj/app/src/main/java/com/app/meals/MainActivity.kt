package com.app.meals

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.compose.material3.windowsizeclass.ExperimentalMaterial3WindowSizeClassApi
import androidx.compose.material3.windowsizeclass.calculateWindowSizeClass
import androidx.compose.runtime.collectAsState
import androidx.compose.runtime.getValue
import androidx.lifecycle.viewmodel.compose.viewModel
import com.app.meals.ui.MealsApp
import com.app.meals.ui.theme.MealsAppTheme
import com.app.meals.ui.viewModels.AppViewModel

class MainActivity : ComponentActivity() {
    @OptIn(ExperimentalMaterial3WindowSizeClassApi::class)
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        setContent {
            val appViewModel: AppViewModel = viewModel(factory = AppViewModel.Factory)
            val isDarkMode by appViewModel.isDarkMode.collectAsState()

            MealsAppTheme(darkTheme = isDarkMode) {
                val windowSize = calculateWindowSizeClass(this)
                MealsApp(windowSizeClass = windowSize.widthSizeClass)
            }
        }
    }
}

// TODO: Final Report
// TODO: Verify checkbox list to make sure there isn't any topic missing